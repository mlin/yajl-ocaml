open OUnit
open Batteries_uni
open Printf

(* To test the YAJL bindings we will generate a list of the events we observe, and compare the
   results with the expected list for various inputs. *)
type event = 
  | Null
  | Bool of bool
  | Int of int
  | Int64 of Int64.t
  | Float of float
  | Number of string
  | String of string
  | Start_map
  | Map_key of string
  | End_map
  | Start_array
  | End_array

let on_null lst = Null :: lst
let on_bool lst b = (Bool b) :: lst
let on_int lst i = (Int i) :: lst
let on_int64 lst i = (Int64 i) :: lst
let on_float lst x = (Float x) :: lst
let on_number lst buf ofs len = (Number (String.sub buf ofs len)) :: lst
let on_string lst buf ofs len = (String (String.sub buf ofs len)) :: lst
let on_start_map lst = Start_map :: lst
let on_map_key lst buf ofs len = (Map_key (String.sub buf ofs len)) :: lst
let on_end_map lst = End_map :: lst
let on_start_array lst = Start_array :: lst
let on_end_array lst = End_array :: lst

(* YAJL callbacks for the three different modes for parsing numbers *)
let raw_callbacks = {
  YAJL.on_null = on_null;
  on_bool = on_bool;
  on_number = `Raw_numbers on_number;
  on_string = on_string;
  on_start_map = on_start_map;
  on_map_key = on_map_key;
  on_end_map = on_end_map;
  on_start_array = on_start_array;
  on_end_array = on_end_array
}
let int_callbacks = { raw_callbacks with YAJL.on_number = `Parse_numbers ((`Int on_int), on_float) }
let int64_callbacks = { raw_callbacks with YAJL.on_number = `Parse_numbers ((`Int64 on_int64), on_float) }

(* Basic tests with a simple JSON: make sure we get all events, and try the three different modes
   for parsing numbers *)
module Basic = struct
  let json = "{\"foo\": [null,false,true,0,12345,3.14159,6e23,\"bar\"]}"
  let parse_json parser = List.rev (YAJL.complete_parse parser (YAJL.parse parser [] json))

  let raw () = 
    let correct =
      [Start_map;
        Map_key "foo";
          Start_array;
            Null; Bool false; Bool true;
            Number "0"; Number "12345"; Number "3.14159"; Number "6e23";
            String "bar";
          End_array;
       End_map]
    assert_equal ~printer:dump correct (parse_json (YAJL.make_parser raw_callbacks))
      

  let int () = 
    let correct =
      [Start_map;
        Map_key "foo";
          Start_array;
            Null; Bool false; Bool true;
            Int 0; Int 12345; Float 3.14159; Float 6e23;
            String "bar";
          End_array;
       End_map]
    assert_equal ~printer:dump correct (parse_json (YAJL.make_parser int_callbacks))
  
  let int64 () =
    let correct =
      [Start_map;
        Map_key "foo";
          Start_array;
            Null; Bool false; Bool true;
            Int64 0L; Int64 12345L; Float 3.14159; Float 6e23;
            String "bar";
          End_array;
       End_map]
    assert_equal ~printer:dump correct (parse_json (YAJL.make_parser int64_callbacks))

  let tests = [
    "raw numbers" >:: raw;
    "ints and floats" >:: int;
    "int64s and floats" >:: int64
  ]

(* Test giving JSON to YAJL in multiple parts *)
module MultiBuffer = struct
  let json = "{\"foo\": [\"Lorem\", \"ipsum\", \"dolor\", \"sit\", \"amet\"]}"
  let json_len = String.length json
  let json_half1 = String.sub json 0 (json_len/2)
  let json_half2 = String.sub json (json_len/2)  (String.length json - json_len/2)

  let halves () =
    let parser = YAJL.make_parser raw_callbacks
    let inter1 = YAJL.parse parser [] json_half1

    assert_equal ~printer:dump
      [Start_map; Map_key "foo"; Start_array; String "Lorem"; String "ipsum"]
      List.rev inter1

    let inter2 = YAJL.parse parser inter1 json_half2
    let rslt = List.rev (YAJL.complete_parse parser inter2)

    assert_equal ~printer:dump
      [Start_map; Map_key "foo"; Start_array;
        String "Lorem"; String "ipsum"; String "dolor"; String "sit"; String "amet";
       End_array; End_map]
      rslt

  (* TODO: more elaborate multipart tests; test giving partial buffers (ofs>0) *)

  let tests = [
    "two halves" >:: halves
  ]

(* Test handling of unrepresentable JSON numbers *)
module NumberOverflow = struct
  let assert_integer_overflow f =
    try
      ignore (f ())
      "no exception raised" @? false
    with
      | YAJL.Parse_error msg ->
        try
          ignore (String.find msg "integer overflow")
          ()
        with
          | Not_found -> ("other-than-expected Parse_error raised: " ^ msg) @? false
      | exn -> raise exn

  let above_int64_max_int () = 
    let n = sprintf "%s0" (Int64.to_string Int64.max_int)
    let json = sprintf "{\"foo\": %s}" n
    assert_integer_overflow (fun () -> YAJL.parse (YAJL.make_parser int_callbacks) [] json)
    assert_integer_overflow (fun () -> YAJL.parse (YAJL.make_parser int64_callbacks) [] json)
    assert_equal ~printer:dump
      [Start_map; Map_key "foo"; Number n; End_map] 
      List.rev (YAJL.parse (YAJL.make_parser raw_callbacks) [] json)

  let below_int64_min_int () = 
    let n = sprintf "%s0" (Int64.to_string Int64.min_int)
    let json = sprintf "{\"foo\": %s}" n
    assert_integer_overflow (fun () -> YAJL.parse (YAJL.make_parser int_callbacks) [] json)
    assert_integer_overflow (fun () -> YAJL.parse (YAJL.make_parser int64_callbacks) [] json)
    assert_equal ~printer:dump
      [Start_map; Map_key "foo"; Number n; End_map] 
      List.rev (YAJL.parse (YAJL.make_parser raw_callbacks) [] json)

  let above_ocaml_max_int () = 
    let n = Int64.succ (Int64.of_int Pervasives.max_int)
    let ns = Int64.to_string n
    let json = sprintf "{\"foo\": %s}" ns
    assert_integer_overflow (fun () -> YAJL.parse (YAJL.make_parser int_callbacks) [] json)
    assert_equal ~printer:dump
      [Start_map; Map_key "foo"; Int64 n; End_map] 
      List.rev (YAJL.parse (YAJL.make_parser int64_callbacks) [] json)
    assert_equal ~printer:dump
      [Start_map; Map_key "foo"; Number ns; End_map] 
      List.rev (YAJL.parse (YAJL.make_parser raw_callbacks) [] json)

  let below_ocaml_min_int () = 
    let n = Int64.pred (Int64.of_int Pervasives.min_int)
    let ns = Int64.to_string n
    let json = sprintf "{\"foo\": %s}" ns
    assert_integer_overflow (fun () -> YAJL.parse (YAJL.make_parser int_callbacks) [] json)
    assert_equal ~printer:dump
      [Start_map; Map_key "foo"; Int64 n; End_map] 
      List.rev (YAJL.parse (YAJL.make_parser int64_callbacks) [] json)
    assert_equal ~printer:dump
      [Start_map; Map_key "foo"; Number ns; End_map] 
      List.rev (YAJL.parse (YAJL.make_parser raw_callbacks) [] json)

  let tests = [
    "above Int64.max_int" >:: above_int64_max_int;
    "below Int64.min_int" >:: below_int64_min_int;
    "above ocaml max_int" >:: above_ocaml_max_int;
    "below ocaml min_int" >:: below_ocaml_min_int;
  ]

module ParserOptions = struct
  (* TODO: test setting parser options *)
  let tests = []

module CallbackExceptions = struct
  (* TODO: test raising exceptions from within callbacks *)
  let tests = []

let all_tests = ("yajl-ocaml tests" >::: [
    "basic" >::: Basic.tests;
    "multiple buffers" >::: MultiBuffer.tests;
    "integer oveflows" >::: NumberOverflow.tests;
    "parser options" >::: ParserOptions.tests;
    "raising exceptions in callbacks" >::: CallbackExceptions.tests
])

run_test_tt ~verbose:true all_tests