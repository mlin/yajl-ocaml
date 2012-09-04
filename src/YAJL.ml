type 'a int_callback = [ `Int of ('a -> int -> 'a) | `Int64 of ('a -> Int64.t -> 'a) ]

type 'a float_callback = 'a -> float -> 'a

type 'a number_callbacks = [
    `Parse_numbers of (('a int_callback)*('a float_callback))
  | `Raw_numbers of ('a -> string -> int -> int -> 'a)
]

type 'a callbacks = {
  on_null : 'a -> 'a;
  on_bool : 'a -> bool -> 'a;
  on_number : 'a number_callbacks;
  on_string : 'a -> string -> int -> int -> 'a;
  on_start_map : 'a -> 'a;
  on_map_key : 'a -> string -> int -> int -> 'a;
  on_end_map : 'a -> 'a;
  on_start_array : 'a -> 'a;
  on_end_array : 'a -> 'a
}

type 'a c_parser

type 'a parser = {
  callbacks : 'a callbacks;
  c_parser : 'a c_parser
}

type parser_options = [
    `Allow_comments
  | `Dont_validate_strings
  | `Allow_trailing_garbage
  | `Allow_multiple_values
  | `Allow_partial_values
]

(* just values 0,1,2 to give to C stub so it knows which number callbacks to use *)
type number_mode =
  | Parse_with_int
  | Parse_with_int64
  | Raw

external yajl_ocaml_make : number_mode -> 'a c_parser = "yajl_ocaml_make"
external yajl_ocaml_free : 'a c_parser -> unit = "yajl_ocaml_free"
external yajl_ocaml_config : 'a c_parser -> int -> int -> unit = "yajl_ocaml_config"

let make_parser ?(options=[]) cbs =
  let mode = match cbs.on_number with
    | `Parse_numbers ((`Int _), _) -> Parse_with_int
    | `Parse_numbers ((`Int64 _), _) -> Parse_with_int64
    | `Raw_numbers _ -> Raw
  let c_parser = yajl_ocaml_make mode
  Gc.finalise yajl_ocaml_free c_parser
  List.iter
    fun opt ->
      let n = match opt with
        | `Allow_comments -> 0x01
        | `Dont_validate_strings -> 0x02
        | `Allow_trailing_garbage -> 0x04
        | `Allow_multiple_values -> 0x08
        | `Allow_partial_values -> 0x10
      yajl_ocaml_config c_parser n 1
    options
  { callbacks = cbs; c_parser = c_parser }

(* internal context value used by our dispatch functions during a parse operation (separate from
   user's context value) *)
type 'a dispatch_context = {
  cbs : 'a callbacks;
  mutable ctx : 'a;
  mutable exn : exn option
}

(* OCaml dispatch functions: the C callbacks (invoked by YAJL) will invoke these OCaml callbacks,
   which in turn dispatch the events to the user's callbacks. The dispatch functions also take care
   of updating the user's context value and buffering any exception raised by the user's callbacks.
   *)
Callback.register "yajl_ocaml_dispatch_null" 
  fun dsp -> try dsp.ctx <- dsp.cbs.on_null dsp.ctx; true with exn -> dsp.exn <- Some exn; false
Callback.register "yajl_ocaml_dispatch_bool"
  fun dsp b -> try dsp.ctx <- dsp.cbs.on_bool dsp.ctx b; true with exn -> dsp.exn <- Some exn; false
Callback.register "yajl_ocaml_dispatch_int"
  fun dsp i ->
    try
      match dsp.cbs.on_number with
        | `Parse_numbers ((`Int f), _) -> dsp.ctx <- f dsp.ctx i; true
        | _ -> assert false
    with exn -> dsp.exn <- Some exn; false
Callback.register "yajl_ocaml_dispatch_int64"
  fun dsp i ->
    try
      match dsp.cbs.on_number with
        | `Parse_numbers ((`Int64 f), _) -> dsp.ctx <- f dsp.ctx i; true
        | _ -> assert false
    with exn -> dsp.exn <- Some exn; false
Callback.register "yajl_ocaml_dispatch_float"
  fun dsp x ->
    try
      match dsp.cbs.on_number with
        | `Parse_numbers (_, f) -> dsp.ctx <- f dsp.ctx x; true
        | _ -> assert false
    with exn -> dsp.exn <- Some exn; false
Callback.register "yajl_ocaml_dispatch_number"
  fun dsp buf ofs len ->
    try
      match dsp.cbs.on_number with
        | `Raw_numbers f -> dsp.ctx <- f dsp.ctx buf ofs len; true
        | _ -> assert false
    with exn -> dsp.exn <- Some exn; false
Callback.register "yajl_ocaml_dispatch_string"
  fun dsp buf ofs len -> try dsp.ctx <- dsp.cbs.on_string dsp.ctx buf ofs len; true with exn -> dsp.exn <- Some exn; false
Callback.register "yajl_ocaml_dispatch_start_map"
  fun dsp -> try dsp.ctx <- dsp.cbs.on_start_map dsp.ctx; true with exn -> dsp.exn <- Some exn; false
Callback.register "yajl_ocaml_dispatch_map_key"
  fun dsp buf ofs len -> try dsp.ctx <- dsp.cbs.on_map_key dsp.ctx buf ofs len; true with exn -> dsp.exn <- Some exn; false
Callback.register "yajl_ocaml_dispatch_end_map"
  fun dsp -> try dsp.ctx <- dsp.cbs.on_end_map dsp.ctx; true with exn -> dsp.exn <- Some exn; false
Callback.register "yajl_ocaml_dispatch_start_array"
  fun dsp -> try dsp.ctx <- dsp.cbs.on_start_array dsp.ctx; true with exn -> dsp.exn <- Some exn; false
Callback.register "yajl_ocaml_dispatch_end_array"
  fun dsp -> try dsp.ctx <- dsp.cbs.on_end_array dsp.ctx; true with exn -> dsp.exn <- Some exn; false

exception Parse_error of string
Callback.register_exception "yajl_ocaml_parse_error" (Parse_error "parse error: integer overflow")

external yajl_ocaml_parse : 'a c_parser -> 'a dispatch_context -> string -> int -> int -> unit = "yajl_ocaml_parse"

let parse ?(ofs=0) ?len parser ctx buf =
  if (ofs < 0 || ofs > String.length buf - 1) then invalid_arg "YAJL.parse: ofs"
  let maxlen = String.length buf - ofs
  let len = match len with
    | None -> maxlen
    | Some k when k >= 0 && k <= maxlen -> k
    | _ -> invalid_arg "YAJL.parse: len"
  if len = 0 then ctx
  else
    (* initialize dispatch_context *)
    let dsp = {
      cbs = parser.callbacks;
      ctx = ctx;
      exn = None
    }
    (* let C stubs and YAJL do their thing *)
    yajl_ocaml_parse parser.c_parser dsp buf ofs len
    (* re-raise any exception raised by the user's callbacks; otherwise return the updated user
       context value. *)
    match dsp.exn with
      | Some exn -> raise exn 
      | None -> dsp.ctx


external yajl_ocaml_complete_parse : 'a c_parser -> 'a dispatch_context -> unit = "yajl_ocaml_complete_parse"

let complete_parse parser ctx =
  let dsp = {
    cbs = parser.callbacks;
    ctx = ctx;
    exn = None
  }
  yajl_ocaml_complete_parse parser.c_parser dsp
  match dsp.exn with
    | Some exn -> raise exn
    | None -> dsp.ctx

