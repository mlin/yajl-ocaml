open Batteries_uni

type t = [
    `Null
  | `Bool of bool
  | `Int of int
  | `Float of float
  | `String of string
  | `Object of (string,t) Map.t
  | `Array of t Vect.t
]

type parser_option = [
    `Allow_comments
  | `Dont_validate_strings
  | `Allow_trailing_garbage
]

module Parser = struct
  type intermediate_state =
    | K of (string,t) Map.t           (* K(map) => expecting next map key to add to map *)
    | V of string * (string,t) Map.t  (* V(key,map) => expecting a value to add to map with this key *)
    | A of t Vect.t                   (* A(vec) => expecting a value to append to vec *)
  type intermediate = intermediate_state list

  let on_value expect x = match expect with
    | V(key,sofar) :: stack -> K(Map.add key x sofar) :: stack
    | A(sofar) :: stack -> A(Vect.append x sofar) :: stack
    | _ -> assert false

  let empty = ""
  let on_null expect = on_value expect `Null
  let on_bool expect b = on_value expect (`Bool b)
  let on_int expect n = on_value expect (`Int n)
  let on_float expect x = on_value expect (`Float x)
  let on_string expect buf ofs len = on_value expect (`String (String.sub buf ofs len))
  let on_start_map expect = K(Map.empty) :: expect
  let on_map_key expect buf ofs len = match expect with
    | K(sofar) :: stack -> V((String.sub buf ofs len),sofar) :: stack
    | _ -> assert false
  let on_end_map = function
    | K(map) :: V(key,ptmap) :: stack -> K(Map.add key (`Object map) ptmap) :: stack
    | K(map) :: A(ptvec) :: stack -> A(Vect.append (`Object map) ptvec) :: stack
    | [K(map)] as top -> top
    | _ -> assert false
  let on_start_array expect = A(Vect.empty) :: expect
  let on_end_array = function
    | A(vec) :: V(key,ptmap) :: stack -> K(Map.add key (`Array vec) ptmap) :: stack
    | A(vec) :: A(ptvec) :: stack -> A(Vect.append (`Array vec) ptvec) :: stack
    | [A(vec)] as top -> top
    | _ -> assert false

  let callbacks = {
    YAJL.on_null = on_null;
    on_bool = on_bool;
    on_number = (`Parse_numbers (`Int on_int, on_float));
    on_string = on_string;
    on_start_map = on_start_map;
    on_map_key = on_map_key;
    on_end_map = on_end_map;
    on_start_array = on_start_array;
    on_end_array = on_end_array
  }

  let make ?(options=([]:parser_option list)) () = YAJL.make_parser ~options:(options :> YAJL.parser_option list) callbacks []

  let complete parser = match YAJL.complete_parse parser with
    | [K(top)] -> `Object top
    | [A(top)] -> `Array top
    | _ -> failwith "JSON.Parser.complete_parse: No data or top level was not an object or array"

type intermediate = Parser.intermediate
let make_parser = Parser.make
let complete_parse = Parser.complete

let parse ?options ?ofs ?len ?pinned buf =
  let parser = Parser.make ?options ()
  YAJL.parse ?ofs ?len ?pinned parser buf
  Parser.complete parser :> t

let parse_file ?options fn =
  File.with_file_in fn (fun infile -> parse ?options (IO.read_all infile))

let gen_cps yajl (json:t) kont =
  let rec gen_value_cps v kont = match v with
    | `Null -> YAJL.gen_null yajl; kont ()
    | `Bool b -> YAJL.gen_bool yajl b; kont ()
    | `Int i -> YAJL.gen_int yajl i; kont ()
    | `Float x -> YAJL.gen_float yajl x; kont ()
    | `String s -> YAJL.gen_string yajl s; kont ()
    | `Object map ->
      YAJL.gen_start_map yajl
      gen_object_cps map
        fun () -> YAJL.gen_end_map yajl; kont ()
    | `Array vec ->
      YAJL.gen_start_array yajl
      gen_array_cps vec
        fun () -> YAJL.gen_end_array yajl; kont ()
  and gen_object_cps map kont =
    if Map.is_empty map then kont ()
    else
      let ((k,v),rest) = Map.pop map
      YAJL.gen_string yajl k
      gen_value_cps v (fun () -> gen_object_cps rest kont)
  and gen_array_cps vec kont =
    if Vect.length vec = 0 then kont ()
    else
      let (fst,rest) = Vect.shift vec
      gen_value_cps fst (fun () -> gen_array_cps rest kont)

  gen_value_cps json kont

let gen yajl json = gen_cps yajl json (fun () -> ())

let generate ?options json =
  let yajl = YAJL.make_gen ?options ()
  gen yajl json
  let buf, ofs, len = YAJL.gen_get_buf yajl
  if ofs=0 && len=(String.length buf) then buf
  else String.sub buf ofs len

let generate_file ?options fn json =
  let yajl = YAJL.make_gen ?options ()
  gen yajl json
  let buf, ofs, len = YAJL.gen_get_buf yajl
  File.with_file_out fn (fun outfile -> ignore (IO.really_output outfile buf ofs len))

module Ops = struct
  exception JSON_type_mismatch of string*t
  exception JSON_not_found of string*t

  let json_bool = function
    | `Bool b -> b
    | json -> raise (JSON_type_mismatch ("Bool",json))
  let json_int = function
    | `Int i -> i
    | json -> raise (JSON_type_mismatch ("Bool",json))
  let json_float = function
    | `Float x -> x
    | json -> raise (JSON_type_mismatch ("Float",json))
  let json_float' = function
    | `Float x -> x
    | `Int i -> float i
    | json -> raise (JSON_type_mismatch ("Float",json))
  let json_number = function
    | (`Int _) as x -> x
    | (`Float _) as x -> x
    | json -> raise (JSON_type_mismatch ("number",json))
  let json_string = function
    | `String s -> s
    | json -> raise (JSON_type_mismatch ("String",json))
  let json_object = function
    | `Object map -> map
    | json -> raise (JSON_type_mismatch ("Object",json))
  let json_object_keys = function
    | `Object map -> List.of_enum (Map.keys map)
    | json -> raise (JSON_type_mismatch ("Object",json))
  let json_array = function
    | `Array items -> items
    | json -> raise (JSON_type_mismatch ("Array",json))
  let json_array_length = function
    | `Array items -> Vect.length items
    | json -> raise (JSON_type_mismatch ("Array",json))


  let ($) json key =
    try
        Map.find key (json_object json)
    with
      | Not_found -> raise (JSON_not_found (key,json))
  let ($?) json key = Map.mem key (json_object json)
  let ($!) json (key,v) = `Object (Map.add key v (json_object json))
  let ($-) json key = `Object (Map.remove key (json_object json))
  let ($@) json k = Vect.get (json_array json) k
  let ($@!) json (k,v) = `Array (Vect.set (json_array json) k v)

