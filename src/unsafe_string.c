
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>


/*
Synthesize an OCaml string of the given length within some malloc'd memory.

Here be dragons! The value can be passed to OCaml code, but if it's ever freed
using free_unsafestring below, any OCaml code that references it is unsafe.
(The OCaml garbage collector can tell that it's not in the OCaml heap, and
will not do anything to it.)

Based on mlvalues.h, alloc.h, and ocamlnet's Netsys_mem.

Important: compare, comparison/equality operators, hashing, and Marshal will
not work with the synthesized string.
http://caml.inria.fr/pub/ml-archives/caml-list/2006/09/977818689f4ceb2178c592453df7a343.en.html
*/
value malloc_unsafe_string(size_t len) {
  mlsize_t wosize;
  void *p;
  value s;

  /* Check max string length (done in OCaml code instead)
  if (len > Bsize_wsize(Max_wosize)-1) {
    return (value)0;
  }
  */

  /* The value's wosize is the minimum number of words needed to hold len+1
     bytes */
  wosize = (len + Bsize_wsize(1)) / Bsize_wsize(1);

  /* Allocate a region with an additional prefix word for the header */
  p = malloc(Bsize_wsize(1+wosize));
  if (!p) {
    return (value)0;
  }

  /* Set the header */
  *((header_t*)p) = ((header_t) wosize << 10) | /* Caml_white | (=0) */ String_tag;

  /* Interpret everything after the header as the string value */
  s = Val_bp(((header_t*)p) + 1);

  /* Initialize the last word to zero */
  Field(s,wosize-1) = 0;

  /*
  Set the last byte of the buffer to an offset value that the runtime uses
  along with wosize to recover the exact byte length of the string. The offset
  value is one less than the number of bytes after the end of the string
  available in the buffer.

  This clever representation provides the following property: if there's only
  one extra byte, it is set to zero, which doubles as a null-terminator for
  the string. Otherwise, the very last byte of the buffer is a nonzero offset,
  but by initializing the entire last word to zero above, we ensured there
  will still be one or more null bytes after the end of the string to provide
  the C terminator.
  */
  Bp_val(s)[Bsize_wsize(wosize)-1] = Bsize_wsize(wosize) - len - 1;

  return s;
}

void free_unsafe_string(value s) {
  free(&Field(s,-1));
}

value unsafe_string_alloc_stub(value len) {
  CAMLparam1(len);
  CAMLlocal1(s);

  s = malloc_unsafe_string(Int_val(len));

  if (!s) {
    caml_failwith("yajl_ocaml_alloc_string");
  }

  CAMLreturn(s);
}

value unsafe_string_free_stub(value s) {
  CAMLparam1(s);
  free_unsafe_string(s);
  CAMLreturn(Val_unit);
}

