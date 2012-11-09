external alloc_stub : int -> string = "unsafe_string_alloc_stub"
let create len =
  if len < 0 || len > Sys.max_string_length then invalid_arg "UnsafeString.create"
  alloc_stub len

external free_stub : string -> unit = "unsafe_string_free_stub"
let destroy s =
  free_stub s
