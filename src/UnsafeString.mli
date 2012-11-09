(** Strings allocated outside of the OCaml heap

Such strings cannot be moved by the OCaml garbage collector, making them more
useful as buffers to be passed between C and OCaml. *)

(** Create a string of the specified size. The string initially contains
arbitrary characters. The string can be used normally by OCaml code with the
major exceptions that the equality, comparison, hashing, and marshalling
primitives will not work with them.

@raise Invalid_argument if [n < 0] or [n > Sys.max_string_length]
@raise Failure if memory allocation fails  *)
val create : int -> string

(** Free the memory underlying the given string value, which must have been
allocated using [create]. After the string is destroyed, {e anything can
happen} if it is ever again referenced from OCaml code. {e Anything} can also
happen if the provided string was not allocated by [create]. *)
val destroy : string -> unit
