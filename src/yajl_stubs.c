#include <stdlib.h>
#include <memory.h>
#include <assert.h>

#include <yajl/yajl_common.h>
#include <yajl/yajl_parse.h>
#include <yajl/yajl_gen.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/custom.h>

/* Internal context for an in-progress parse operation */
struct op_context {
  value *dsp_ctx;       /* the OCaml context value to give the OCaml dispatch callbacks during
                           the operation (not the same as the user's context value, which is
                           inside it) */

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
More elaborate logic for when we have to pass along a buffer (in string,offset,length form):
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
  } else { \
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
  if (len>0) {
    BEGIN_DISPATCH()
    DISPATCH_BUFFER(string)
  }
  else {
    BEGIN_DISPATCH()
    DISPATCH_UNIT(empty_string)
  }
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
  assert (!p->op.buf);
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

value yajl_ocaml_parse(value box, value dsp, value buf, value ofs, value len, value pinned) {
  CAMLparam5(box, dsp, buf, ofs, len);
  CAMLxparam1(pinned);
  unsigned char *errmsg = NULL;
  CAMLlocal1(camlerr);
  struct parser *p = Parser_val(box);

  p->op.bufsz = (size_t) Long_val(len);
  if (!Bool_val(pinned)) {
    /* Since the OCaml GC is liable to move String_val(buf) around during the operation, we
       cannot just give that to YAJL. Instead we must make our own copy of it. */
    p->op.buf = (unsigned char*) malloc(p->op.bufsz);
    memcpy(p->op.buf, (String_val(buf)+Int_val(ofs)), p->op.bufsz);
  } else {
    /* The caller promises that the GC won't move buf around during the operation (they
       somehow allocated it outside of the OCaml heap), so we don't have to copy it. */
    p->op.buf = (unsigned char*) String_val(buf) + Int_val(ofs);
  }

  /* Also store a pointer to the buf value for this operation, which the above C callbacks can
     reference in order to avoid further, unnecessary copying of the data */
  p->op.orig_buf = &buf;
  p->op.orig_buf_ofs = Int_val(ofs);

  /* set the OCaml context value for this operation (which is actually the
     OCaml representation of the parser). The C callbacks will pass this back
     to the OCaml dispatch functions. */
  if (p->op.dsp_ctx) { caml_failwith("Nested invocation of YAJL.parse"); }
  p->op.dsp_ctx = &dsp;

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
  if (!Bool_val(pinned)) {
    free(p->op.buf);
  }
  memset(&(p->op), 0, sizeof(struct op_context));

  if (errmsg) {
    camlerr = caml_copy_string((char*)errmsg);
    yajl_free_error(p->yajl, errmsg);

    caml_raise_with_arg(*caml_named_value("yajl_ocaml_parse_error"), camlerr);
  }

  CAMLreturn(Val_unit);
}

value yajl_ocaml_parse_byte(value *args, int n) {
  assert(n == 6);
  return yajl_ocaml_parse(args[0], args[1], args[2], args[3], args[4], args[5]);
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


/* GENERATOR STUBS */


struct gen {
  yajl_gen yajl;
  unsigned char *indent_string;
};


static struct custom_operations ocaml_yajl_gen_ops = {
  "ocaml_yajl_gen",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

#define Gen_val(v) (* ((struct gen **) Data_custom_val(v)))

value yajl_ocaml_make_gen() {
  CAMLparam0();
  CAMLlocal1(box);

  /* allocate gen struct */
  struct gen *p = malloc(sizeof(struct gen));
  memset(p, 0, sizeof(struct gen));

  /* instantiate YAJL generator */
  p->yajl = yajl_gen_alloc(NULL);
  
  /* allocate a value box for the parser struct and return it */
  box = alloc_custom(&ocaml_yajl_gen_ops, sizeof(struct gen *), 0, 1);
  Gen_val(box) = p;
  CAMLreturn(box);
}

value yajl_ocaml_gen_config(value box, value opt, value val) {
  CAMLparam3(box, opt, val);
  struct gen *p = Gen_val(box);

  if (!yajl_gen_config(p->yajl, Int_val(opt), Int_val(val))) {
    caml_failwith("YAJL.make_gen: error setting option(s)");
  }

  CAMLreturn(Val_unit);
}

value yajl_ocaml_gen_config_indent_string(value box, value string) {
  CAMLparam2(box, string);
  struct gen *p = Gen_val(box);

  if (p->indent_string) {
    caml_failwith("YAJL.make_gen: multiple `Beautify options");
  }

  p->indent_string = malloc(caml_string_length(string)+1);
  p->indent_string[caml_string_length(string)] = 0;
  memcpy(p->indent_string, String_val(string), caml_string_length(string));

  if (!yajl_gen_config(p->yajl, yajl_gen_indent_string, p->indent_string)) {
    caml_failwith("YAJL.make_gen: error setting indent string");
  }

  CAMLreturn(Val_unit);
}

value yajl_ocaml_gen_free(value box) {
  CAMLparam1(box);

  struct gen *p = Gen_val(box);
  assert(p != NULL);
  yajl_gen_free(p->yajl);
  if (p->indent_string) {
    free(p->indent_string);
  }
  free(p);

  CAMLreturn(Val_unit);
}

value yajl_ocaml_gen_get_buf(value box) {
  CAMLparam1(box);
  CAMLlocal1(ans);
  struct gen *p = Gen_val(box);
  const unsigned char *buf;
  size_t len;

  switch (yajl_gen_get_buf(p->yajl, &buf, &len)) {
  case yajl_gen_status_ok:
    ans = caml_alloc_tuple(3);
    Store_field(ans, 0, caml_copy_string(buf));
    Store_field(ans, 1, Val_int(0));
    Store_field(ans, 2, Val_int(len));
    break;
    /*
      TODO: We should be able to avoid copying the generator buffer by
      supplying YAJL with custom allocator functions that use
      caml_alloc_string.
    */
  default:
    caml_failwith("YAJL.gen_get_buf");
  }

  CAMLreturn(ans);
}

value yajl_ocaml_gen_clear(value box) {
  CAMLparam1(box);
  struct gen *p = Gen_val(box);

  yajl_gen_clear(p->yajl);

  CAMLreturn(Val_unit);
}

#define BEGIN_GEN(yajl_call) struct gen *p = Gen_val(box);\
  switch(yajl_call) { \
  case yajl_status_ok: break;

#define END_GEN(nm) \
  case yajl_max_depth_exceeded: \
    caml_failwith(#nm ": max depth exceeded"); \
    assert(0); /* should not get here */ \
  default: \
    caml_failwith(nm); \
    assert(0); /* should not get here */ \
  } \
  CAMLreturn(Val_unit);

value yajl_ocaml_gen_string(value box, value buf, value ofs, value len) {
  CAMLparam4(box, buf, ofs, len);
  CAMLlocal1(tuple);
  BEGIN_GEN(yajl_gen_string(p->yajl, (unsigned char*) (String_val(buf)+Int_val(ofs)), Int_val(len)))
  case yajl_gen_invalid_string:
    CAMLreturn(Val_int(314159)); /* sentinel value recognized by ocaml stub */
  END_GEN("YAJL.gen_string")
}

value yajl_ocaml_gen_int(value box, value n) {
  CAMLparam2(box, n);
  BEGIN_GEN(yajl_gen_integer(p->yajl, Int_val(n)))
  END_GEN("YAJL.gen_int")
}

value yajl_ocaml_gen_int64(value box, value n) {
  CAMLparam2(box, n);
  BEGIN_GEN(yajl_gen_integer(p->yajl, Int64_val(n)))
  END_GEN("YAJL.gen_int64")
}

value yajl_ocaml_gen_float(value box, value x) {
  CAMLparam2(box, x);
  BEGIN_GEN(yajl_gen_double(p->yajl, Double_val(x)))
  case yajl_gen_invalid_number:
    caml_raise_with_arg(*caml_named_value("yajl_ocaml_gen_invalid_float"), x);
    assert(0);
  END_GEN("YAJL.gen_float")
}

value yajl_ocaml_gen_number(value box, value buf, value ofs, value len) {
  CAMLparam4(box, buf, ofs, len);
  BEGIN_GEN(yajl_gen_number(p->yajl, (unsigned char*) (String_val(buf)+Int_val(ofs)), Int_val(len)))
  END_GEN("YAJL.gen_number")
}

value yajl_ocaml_gen_null(value box) {
  CAMLparam1(box);
  BEGIN_GEN(yajl_gen_null(p->yajl))
  END_GEN("YAJL.gen_null")
}

value yajl_ocaml_gen_bool(value box, value b) {
  CAMLparam2(box, b);
  BEGIN_GEN(yajl_gen_bool(p->yajl, Bool_val(b)))
  END_GEN("YAJL.gen_bool")
}

value yajl_ocaml_gen_start_map(value box) {
  CAMLparam1(box);
  BEGIN_GEN(yajl_gen_map_open(p->yajl))
  END_GEN("YAJL.gen_start_map")
}

value yajl_ocaml_gen_end_map(value box) {
  CAMLparam1(box);
  BEGIN_GEN(yajl_gen_map_close(p->yajl))
  END_GEN("YAJL.gen_end_map")
}

value yajl_ocaml_gen_start_array(value box) {
  CAMLparam1(box);
  BEGIN_GEN(yajl_gen_array_open(p->yajl))
  END_GEN("YAJL.gen_start_array")
}

value yajl_ocaml_gen_end_array(value box) {
  CAMLparam1(box);
  BEGIN_GEN(yajl_gen_array_close(p->yajl))
  END_GEN("YAJL.gen_end_array")
}
