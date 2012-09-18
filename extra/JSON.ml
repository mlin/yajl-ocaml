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

let empty = `Object Map.empty

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

let from_string ?options ?ofs ?len ?pinned buf =
  let parser = Parser.make ?options ()
  YAJL.parse ?ofs ?len ?pinned parser buf
  Parser.complete parser :> t

let from_file ?options fn =
  File.with_file_in fn (fun infile -> from_string ?options (IO.read_all infile))

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

let generate yajl json = gen_cps yajl json (fun () -> ())

let to_string ?options json =
  let yajl = YAJL.make_gen ?options ()
  generate yajl json
  let buf, ofs, len = YAJL.gen_get_buf yajl
  if ofs=0 && len=(String.length buf) then buf
  else String.sub buf ofs len

let to_file ?options fn json =
  let yajl = YAJL.make_gen ?options ()
  generate yajl json
  let buf, ofs, len = YAJL.gen_get_buf yajl
  File.with_file_out fn (fun outfile -> ignore (IO.really_output outfile buf ofs len))

exception Type_mismatch of string*t
exception No_key of string*t 

let bool = function
  | `Bool b -> b
  | json -> raise (Type_mismatch ("Bool",json))
let int = function
  | `Int i -> i
  | json -> raise (Type_mismatch ("Bool",json))
let float = function
  | `Float x -> x
  | json -> raise (Type_mismatch ("Float",json))
let float' = function
  | `Float x -> x
  | `Int i -> float_of_int i
  | json -> raise (Type_mismatch ("Int/Float",json))
let number = function
  | (`Int _) as x -> x
  | (`Float _) as x -> x
  | json -> raise (Type_mismatch ("Int/Float",json))
let string = function
  | `String s -> s
  | json -> raise (Type_mismatch ("String",json))
let obj = function
  | `Object map -> map
  | json -> raise (Type_mismatch ("Object",json))
let obj_or_null = function
  | `Null as x -> (x :> [`Object of (string,t) Map.t | `Null])
  | (`Object _) as x -> (x :> [`Object of (string,t) Map.t | `Null])
  | json -> raise (Type_mismatch ("Object/Null",json))
let obj_keys = function
  | `Object map -> List.of_enum (Map.keys map)
  | json -> raise (Type_mismatch ("Object",json))
let array = function
  | `Array items -> items
  | json -> raise (Type_mismatch ("Array",json))
let array_length = function
  | `Array items -> Vect.length items
  | json -> raise (Type_mismatch ("Array",json))

module Operators = struct
  let ($) json key =
    try
        Map.find key (obj json)
    with
      | Not_found -> raise (No_key (key,json))
  let ($?) json key = Map.mem key (obj json)
  let ($+) json (key,v) = `Object (Map.add key v (obj json))
  let ($-) json key = `Object (Map.remove key (obj json))
  let ($@) json k = Vect.get (array json) k
  let ($@!) json (k,v) = `Array (Vect.set (array json) k v)

let of_assoc lst = List.fold_left Operators.($+) empty lst
let of_list lst = `Array (Vect.of_list lst)
let of_array arr = `Array (Vect.of_array arr)

