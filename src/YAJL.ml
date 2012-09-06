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

type 'a c_parser (* maintained by the C stubs *)

type parser_state =
  | Open
  | Parsing
  | Closed
  | Exception of exn

type 'a parser = {
  callbacks : 'a callbacks;
  c_parser : 'a c_parser;
  mutable state : parser_state;
  mutable ctx : 'a (* user's context value *)
}

exception Parse_error of string
Callback.register_exception "yajl_ocaml_parse_error" (Parse_error "")

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

let make_parser ?(options=[]) cbs ctx =
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
  { callbacks = cbs; c_parser = c_parser; state = Open; ctx = ctx }

(* OCaml dispatch functions: the C callbacks (invoked by YAJL) will invoke
   these OCaml callbacks, which in turn dispatch the events to the user's
   callbacks. The dispatch functions also take care of updating the user's
   context value and buffering any exception raised by the user's callbacks. *)
Callback.register "yajl_ocaml_dispatch_null" 
  fun p -> try p.ctx <- p.callbacks.on_null p.ctx; true with exn -> p.state <- Exception exn; false
Callback.register "yajl_ocaml_dispatch_bool"
  fun p b -> try p.ctx <- p.callbacks.on_bool p.ctx b; true with exn -> p.state <- Exception exn; false
Callback.register "yajl_ocaml_dispatch_int"
  fun p i ->
    try
      match p.callbacks.on_number with
        | `Parse_numbers ((`Int f), _) -> p.ctx <- f p.ctx i; true
        | _ -> assert false
    with exn -> p.state <- Exception exn; false
Callback.register "yajl_ocaml_dispatch_int_overflow"
  fun p i ->
    (* Record exception for an integer that can represented in the C long long
       type, but not the OCaml int type. *)
    p.state <- Exception (Parse_error ("integer overflow: " ^ (Int64.to_string i)))
    false
Callback.register "yajl_ocaml_dispatch_int64"
  fun p i ->
    try
      match p.callbacks.on_number with
        | `Parse_numbers ((`Int64 f), _) -> p.ctx <- f p.ctx i; true
        | _ -> assert false
    with exn -> p.state <- Exception exn; false
Callback.register "yajl_ocaml_dispatch_float"
  fun p x ->
    try
      match p.callbacks.on_number with
        | `Parse_numbers (_, f) -> p.ctx <- f p.ctx x; true
        | _ -> assert false
    with exn -> p.state <- Exception exn; false
Callback.register "yajl_ocaml_dispatch_number"
  fun p buf ofs len ->
    try
      assert (len>0)
      match p.callbacks.on_number with
        | `Raw_numbers f -> p.ctx <- f p.ctx buf ofs len; true
        | _ -> assert false
    with exn -> p.state <- Exception exn; false
Callback.register "yajl_ocaml_dispatch_string"
  fun p buf ofs len -> try p.ctx <- p.callbacks.on_string p.ctx buf ofs len; true with exn -> p.state <- Exception exn; false
Callback.register "yajl_ocaml_dispatch_start_map"
  fun p -> try p.ctx <- p.callbacks.on_start_map p.ctx; true with exn -> p.state <- Exception exn; false
Callback.register "yajl_ocaml_dispatch_map_key"
  fun p buf ofs len -> try p.ctx <- p.callbacks.on_map_key p.ctx buf ofs len; true with exn -> p.state <- Exception exn; false
Callback.register "yajl_ocaml_dispatch_end_map"
  fun p -> try p.ctx <- p.callbacks.on_end_map p.ctx; true with exn -> p.state <- Exception exn; false
Callback.register "yajl_ocaml_dispatch_start_array"
  fun p -> try p.ctx <- p.callbacks.on_start_array p.ctx; true with exn -> p.state <- Exception exn; false
Callback.register "yajl_ocaml_dispatch_end_array"
  fun p -> try p.ctx <- p.callbacks.on_end_array p.ctx; true with exn -> p.state <- Exception exn; false

external yajl_ocaml_parse : 'a c_parser -> 'a parser -> string -> int -> int -> unit = "yajl_ocaml_parse"

let parse ?context ?(ofs=0) ?len parser buf =
  if (ofs < 0 || ofs > String.length buf - 1) then invalid_arg "YAJL.parse: ofs"
  let maxlen = String.length buf - ofs
  let len = match len with
    | None -> maxlen
    | Some k when k >= 0 && k <= maxlen -> k
    | _ -> invalid_arg "YAJL.parse: len"

  match parser.state with
    | Open -> ()
    | Parsing -> failwith "YAJL.parse: overlapping invocations"
    | Closed -> failwith "YAJL.parse: invocation following YAJL.complete_parse"
    | Exception _ -> failwith "YAJL.parse: invocation following a previous exception"

  match context with
    | Some thing -> parser.ctx <- thing
    | None -> ()

  if len > 0 then
    (* let C stubs and YAJL do their thing *)
    parser.state <- Parsing
    yajl_ocaml_parse parser.c_parser parser buf ofs len

    (* re-raise any exception raised by the user's callbacks *)
    match parser.state with
      | Parsing -> parser.state <- Open
      | Exception exn -> raise exn 
      | _ -> assert false

external yajl_ocaml_complete_parse : 'a c_parser -> 'a parser -> unit = "yajl_ocaml_complete_parse"

let identity x = x

let complete_parse ?context ?(t=identity) parser =
  match parser.state with
    | Open -> ()
    | Parsing -> failwith "YAJL.complete_parse: overlapping invocations"
    | Closed -> failwith "YAJL.complete_parse: multiple invocations"
    | Exception _ -> failwith "YAJL.complete_parse: invocation following a previous exception"

  match context with
    | Some thing -> parser.ctx <- thing
    | None -> ()
  
  parser.state <- Parsing
  yajl_ocaml_complete_parse parser.c_parser parser

  (* re-raise any exception raised by the user's callback; otherwise, return
     the final context value *)
  match parser.state with
    | Parsing -> parser.state <- Closed; t parser.ctx
    | Exception exn -> raise exn
    | _ -> assert false

let last_context ?(t=identity) { ctx } = t ctx
