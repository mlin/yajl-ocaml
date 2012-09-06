#include <stdlib.h>
#include <memory.h>
#include <assert.h>

#include <yajl/yajl_common.h>
#include <yajl/yajl_parse.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/custom.h>

/* Internal context for an in-progress prase operation */
struct op_context {
  value *dsp_ctx;       /* the OCaml dispatch_context for the operation */

  value *orig_buf;      /* OCaml buffer (string) given to the parser */
  size_t orig_buf_ofs;  /* offset into orig_buf given to the parser */
  unsigned char *buf;   /* our pinned copy of the buffer (starting at orig_buf_ofs) */
  size_t bufsz;         /* size of the region being parsed */
};

struct parser {
  yajl_callbacks cbs;
  yajl_handle yajl;
  struct op_context op;
};

/*

YAJL CALLBACKS

These C callbacks invoke corresponding OCaml dispatch functions, which in turn invoke the
user's OCaml callback functions. If the user's OCaml callback raises an exception, the dispatch
function returns false, in which case our C callbacks also return false, causing YAJL to stop
parsing (client_canceled). The OCaml bindings code will take care of re-raising the user's
exception in such a case.

*/

/* Boilerplate macros for C callbacks; take care of locating the corresponding OCaml dispatch
   function, invoking it and returning to YAJL */
#define BEGIN_DISPATCH() \
  CAMLparam0(); \
  CAMLlocal1(ans); \
  struct parser *p = (struct parser*) ctx; \
  static value *dsp = NULL;

#define RESOLVE(nm) \
  if (!dsp) { dsp = caml_named_value("yajl_ocaml_dispatch_" #nm); } \
  assert(dsp);

#define DISPATCH_UNIT(nm) \
  RESOLVE(nm) \
  ans = caml_callback(*dsp, *(p->op.dsp_ctx)); \
  CAMLreturnT(int,Bool_val(ans));

#define DISPATCH_VALUE(nm,v) \
  RESOLVE(nm) \
  ans = caml_callback2(*dsp, *(p->op.dsp_ctx), v); \
  CAMLreturnT(int,Bool_val(ans));

/*
More elaborate logic for when we have to pass along a buffer (in string+offset+length form):
If YAJL has given us data within p->buf, give OCaml the corresponding region within p->orig_buf.
Otherwise, allocate a new OCaml string and copy the data into it.
(see how these are initialized in yajl_ocaml_parse, below)
*/
#define DISPATCH_BUFFER(nm) \
  CAMLlocalN(args,4); \
  assert(buf); \
  args[0] = *(p->op.dsp_ctx); \
  args[3] = Val_int(len); \
  if ((unsigned char*) buf >= p->op.buf && ((unsigned char*)buf+len) <= (p->op.buf+p->op.bufsz)) { \
    args[1] = *(p->op.orig_buf); \
    args[2] = Val_int(p->op.orig_buf_ofs + ((unsigned char*)buf-p->op.buf)); \
  } \
  else { \
    args[1] = caml_alloc_string(len); \
    memcpy(String_val(args[1]), buf, len); \
    args[2] = Val_int(0); \
  } \
  RESOLVE(nm) \
  ans = caml_callbackN(*dsp, 4, args); \
  CAMLreturnT(int,Bool_val(ans));

int yajl_ocaml_on_null(void *ctx) {
  BEGIN_DISPATCH()
  DISPATCH_UNIT(null)
}

int yajl_ocaml_on_bool(void *ctx, int boolVal) {
  BEGIN_DISPATCH()
  DISPATCH_VALUE(bool, boolVal ? Val_true : Val_false);
}

int yajl_ocaml_on_int(void *ctx, long long i) {
  if (i >= Min_long && i <= Max_long) {
    BEGIN_DISPATCH()
    DISPATCH_VALUE(int, Val_long(i))
  }
  else {
    /* We've encountered an integer that can be represented by C long long, but not OCaml int.
       A special OCaml dispatch function will take care of recording the exception. */
    BEGIN_DISPATCH()
    CAMLlocal1(box);
    box = caml_copy_int64(i);
    DISPATCH_VALUE(int_overflow, box)
  }
}

int yajl_ocaml_on_int64(void *ctx, long long i) {
  BEGIN_DISPATCH()
  CAMLlocal1(box);
  box = caml_copy_int64(i);
  DISPATCH_VALUE(int64, box)
}

int yajl_ocaml_on_float(void *ctx, double x) {
  BEGIN_DISPATCH()
  CAMLlocal1(box);
  box = caml_copy_double(x);
  DISPATCH_VALUE(float, box)
}

int yajl_ocaml_on_number(void *ctx, const char *buf, size_t len) {
  BEGIN_DISPATCH()
  DISPATCH_BUFFER(number)
}

int yajl_ocaml_on_string(void *ctx, const unsigned char *buf, size_t len) {
  BEGIN_DISPATCH()
  DISPATCH_BUFFER(string)
}

int yajl_ocaml_on_start_map(void *ctx) {
  BEGIN_DISPATCH()
  DISPATCH_UNIT(start_map)
}

int yajl_ocaml_on_map_key(void *ctx, const unsigned char *buf, size_t len) {
  BEGIN_DISPATCH()
  DISPATCH_BUFFER(map_key)
}

int yajl_ocaml_on_end_map(void *ctx) {
  BEGIN_DISPATCH()
  DISPATCH_UNIT(end_map)
}

int yajl_ocaml_on_start_array(void *ctx) {
  BEGIN_DISPATCH()
  DISPATCH_UNIT(start_array)
}

int yajl_ocaml_on_end_array(void *ctx) {
  BEGIN_DISPATCH()
  DISPATCH_UNIT(end_array)
}

/*
YAJL/OCAML STUBS
*/

static struct custom_operations ocaml_yajl_parser_ops = {
  "ocaml_yajl_parser",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

#define Parser_val(v) (* ((struct parser **) Data_custom_val(v)))

value yajl_ocaml_make(value number_mode) {
  CAMLparam1(number_mode);
  CAMLlocal1(parser_box);

  /* allocate parser struct */
  struct parser *p = malloc(sizeof(struct parser));
  memset(p, 0, sizeof(struct parser));

  /* fill in our C callbacks, which in turn invoke the OCaml dispatch functions, which lastly
     invoke the user's OCaml callbacks */
  p->cbs.yajl_null = yajl_ocaml_on_null;
  p->cbs.yajl_boolean = yajl_ocaml_on_bool;
  p->cbs.yajl_string = yajl_ocaml_on_string;
  p->cbs.yajl_start_map = yajl_ocaml_on_start_map;
  p->cbs.yajl_map_key = yajl_ocaml_on_map_key;
  p->cbs.yajl_end_map = yajl_ocaml_on_end_map;
  p->cbs.yajl_start_array = yajl_ocaml_on_start_array;
  p->cbs.yajl_end_array = yajl_ocaml_on_end_array;
  /* fill in appropriate callbacks for the number parsing mode */
  switch (Int_val(number_mode)) {
  case 0:
    p->cbs.yajl_integer = yajl_ocaml_on_int;
    p->cbs.yajl_double = yajl_ocaml_on_float;
    break;
  case 1:
    p->cbs.yajl_integer = yajl_ocaml_on_int64;
    p->cbs.yajl_double = yajl_ocaml_on_float;
    break;
  case 2:
    p->cbs.yajl_number = yajl_ocaml_on_number;
    break;
  default:
    assert(0);
  }

  /* instantiate YAJL parser */
  p->yajl = yajl_alloc(&(p->cbs), NULL, p);
  
  /* allocate a value box for the parser struct and return it */
  parser_box = alloc_custom(&ocaml_yajl_parser_ops, sizeof(struct parser *), 0, 1);
  Parser_val(parser_box) = p;
  CAMLreturn(parser_box);
}

value yajl_ocaml_free(value box) {
  CAMLparam1(box);

  struct parser *p = Parser_val(box);
  assert(p != NULL);
  yajl_free(p->yajl);
  if (p->op.buf) { free(p->op.buf); }
  memset(p, 0, sizeof(struct parser));
  free(p);

  CAMLreturn(Val_unit);
}

value yajl_ocaml_config(value box, value opt, value val) {
  CAMLparam3(box, opt, val);
  struct parser *p = Parser_val(box);

  if (!yajl_config(p->yajl, Int_val(opt), Int_val(val))) {
    caml_failwith("YAJL.make_parser: error setting option(s)");
  }

  CAMLreturn(Val_unit);
}

value yajl_ocaml_parse(value box, value dsp, value buf, value ofs, value len) {
  CAMLparam5(box, dsp, buf, ofs, len);
  unsigned char *errmsg = NULL;
  CAMLlocal1(camlerr);
  struct parser *p = Parser_val(box);

  /* set the OCaml dispatch_context value for this operation. The C callbacks will pass this back
     to the OCaml dispatch functions. */
  if (p->op.dsp_ctx) { caml_failwith("Nested invocation of YAJL.parse"); }
  p->op.dsp_ctx = &dsp;

  /* Since the OCaml GC is liable to move String_val(buf) around during the operation, we cannot
     just give that to YAJL. Instead we must make our own copy of it.
     TODO: investigate if there's a way to "pin" buf in-memory so that we don't have to make this
           copy. */
  p->op.bufsz = (size_t) Int_val(len);
  p->op.buf = (unsigned char*) malloc(p->op.bufsz);
  memcpy(p->op.buf, (String_val(buf)+Int_val(ofs)), p->op.bufsz);
  /* Also store a pointer to the buf value for this operation, which the above C callbacks can
     reference in order to avoid further, unnecessary copying of the data */
  p->op.orig_buf = &buf;
  p->op.orig_buf_ofs = Int_val(ofs);

  /* let YAJL do its thing */
  switch(yajl_parse(p->yajl, p->op.buf, p->op.bufsz)) {
  case yajl_status_error:
    errmsg = yajl_get_error(p->yajl, 1, p->op.buf, p->op.bufsz);
    break;

  case yajl_status_ok:
  case yajl_status_client_canceled: 
    /* ocaml code will take care of client_canceled (<=> user's ocaml callbacks raised an
       exception) */
    break;

  default: assert(0);
  }

  /* clean up and raise an OCaml exception for any parser error */
  free(p->op.buf);
  memset(&(p->op), 0, sizeof(struct op_context));

  if (errmsg) {
    camlerr = caml_copy_string((char*)errmsg);
    yajl_free_error(p->yajl, errmsg);

    caml_raise_with_arg(*caml_named_value("yajl_ocaml_parse_error"), camlerr);
  }

  CAMLreturn(Val_unit);
}

value yajl_ocaml_complete_parse(value box, value dsp) {
  CAMLparam2(box, dsp);
  unsigned char *errmsg = NULL;
  CAMLlocal1(camlerr);
  struct parser *p = Parser_val(box);

  if (p->op.dsp_ctx) { caml_failwith("Nested invocation of YAJL.complete_parse"); }
  p->op.dsp_ctx = &dsp;

  switch(yajl_complete_parse(p->yajl)) {
  case yajl_status_error:
    errmsg = yajl_get_error(p->yajl, 0, NULL, 0);
    break;

  case yajl_status_ok:
  case yajl_status_client_canceled: 
    /* ocaml code will take care of client_canceled */
    break;

    default: assert(0);
  }

  p->op.dsp_ctx = NULL;

  if (errmsg) {
    camlerr = caml_copy_string((char*)errmsg);
    yajl_free_error(p->yajl, errmsg);

    caml_raise_with_arg(*caml_named_value("yajl_ocaml_parse_error"), camlerr);
  }

  CAMLreturn(Val_unit);
}