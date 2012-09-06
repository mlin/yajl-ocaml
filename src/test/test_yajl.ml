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

module Basic = struct
  (* Basic tests with a simple JSON: make sure we get all events, and try the
     three different modes for parsing numbers *)

  let json = "{\"foo\": [null,false,true,0,12345,3.14159,6e23,\"bar\",\"\"]}"
  let parse_json parser =
    YAJL.parse parser json
    List.rev (YAJL.complete_parse parser)

  let raw () = 
    let correct =
      [Start_map;
        Map_key "foo";
          Start_array;
            Null; Bool false; Bool true;
            Number "0"; Number "12345"; Number "3.14159"; Number "6e23";
            String "bar"; String "";
          End_array;
       End_map]
    assert_equal ~printer:dump correct (parse_json (YAJL.make_parser raw_callbacks []))
      

  let int () = 
    let correct =
      [Start_map;
        Map_key "foo";
          Start_array;
            Null; Bool false; Bool true;
            Int 0; Int 12345; Float 3.14159; Float 6e23;
            String "bar"; String "";
          End_array;
       End_map]
    assert_equal ~printer:dump correct (parse_json (YAJL.make_parser int_callbacks []))
  
  let int64 () =
    let correct =
      [Start_map;
        Map_key "foo";
          Start_array;
            Null; Bool false; Bool true;
            Int64 0L; Int64 12345L; Float 3.14159; Float 6e23;
            String "bar"; String "";
          End_array;
       End_map]
    assert_equal ~printer:dump correct (parse_json (YAJL.make_parser int64_callbacks []))

  let tests = [
    "raw numbers" >:: raw;
    "ints and floats" >:: int;
    "int64s and floats" >:: int64
  ]

module MultiBuffer = struct
  (* Test giving JSON to YAJL in multiple parts *)

  let json = "{\"foo\": [\"Lorem\", \"ipsum\", \"dolor\", \"sit\", \"amet\"]}"
  let json_len = String.length json
  let json_half1 = String.sub json 0 (json_len/2)
  let json_half2 = String.sub json (json_len/2)  (String.length json - json_len/2)

  let halves () =
    let parser = YAJL.make_parser raw_callbacks []

    YAJL.parse parser json_half1

    assert_equal ~printer:dump
      [Start_map; Map_key "foo"; Start_array; String "Lorem"; String "ipsum"]
      YAJL.last_context ~t:List.rev parser

    YAJL.parse parser json_half2

    assert_equal ~printer:dump
      [Start_map; Map_key "foo"; Start_array;
        String "Lorem"; String "ipsum"; String "dolor"; String "sit"; String "amet";
       End_array; End_map]
      YAJL.complete_parse ~t:List.rev parser

  let override_context_value () =
    let parser = YAJL.make_parser raw_callbacks []

    YAJL.parse parser json_half1

    assert_equal ~printer:dump
      [Start_map; Map_key "foo"; Start_array; String "Lorem"; String "ipsum"]
      YAJL.last_context ~t:List.rev parser

    YAJL.parse ~context:[String "foo"] parser json_half2

    assert_equal ~printer:dump
      [String "foo"; String "dolor"; String "sit"; String "amet"; End_array; End_map]
      YAJL.last_context ~t:List.rev parser

    assert_equal ~printer:dump
      [String "bar"]
      YAJL.complete_parse ~context:[String "bar"] ~t:List.rev parser

  let partial_buffers () =
    let parser = YAJL.make_parser raw_callbacks []

    List.iter
      fun bogus -> assert_raises (Invalid_argument "YAJL.parse: ofs") (fun () -> YAJL.parse parser json_half1 ~ofs:bogus)
      [-1; String.length json_half1]

    List.iter
      fun (ofs,len) -> assert_raises (Invalid_argument "YAJL.parse: len") (fun () -> YAJL.parse parser json_half1 ~ofs ~len)
      [(0,-1); (0,String.length json_half1 + 1); (1,String.length json_half1 + 2)]

    YAJL.parse parser ("0123456789" ^ json_half1) ~ofs:10

    assert_equal ~printer:dump
      [Start_map; Map_key "foo"; Start_array; String "Lorem"; String "ipsum"]
      YAJL.last_context ~t:List.rev parser

    YAJL.parse parser ("0123456789" ^ json_half2 ^ "0123456789") ~ofs:10 ~len:(String.length json_half2)

    assert_equal ~printer:dump
      [Start_map; Map_key "foo"; Start_array;
        String "Lorem"; String "ipsum"; String "dolor"; String "sit"; String "amet";
       End_array; End_map]
      YAJL.complete_parse ~t:List.rev parser

  (* TODO: more elaborate multipart tests *)

  let tests = [
    "two halves" >:: halves;
    "override context value" >:: override_context_value;
    "partial buffers" >:: partial_buffers
  ]

module CallbackExceptions = struct
  exception The_exception

  let trivial () =
    let parser = YAJL.make_parser {raw_callbacks with YAJL.on_start_map = (fun _ -> raise The_exception)} []
    assert_raises The_exception (fun () -> YAJL.parse parser "{}")

  let interruption () =
    let parser = YAJL.make_parser {raw_callbacks with
      YAJL.on_null = (fun _ -> raise The_exception);
    } []
    assert_raises The_exception (fun () -> YAJL.parse parser "{\"foo\": [1,2,null,3,4]}")
    assert_equal ~printer:dump
      [Start_map; Map_key "foo"; Start_array; Number "1"; Number "2"]
      YAJL.last_context ~t:List.rev parser

  let tests = [
    "trivial" >:: trivial;
    "interruption" >:: interruption
  ]

module StateErrors = struct
  (* TODO: test receipt of Failure errors due to calling methods at impermissible times *)

  let tests = []

module ParseErrors = struct
  (* TODO: test receipt of Parse_error when using malformed JSON *)

  let tests = []

module NumberOverflow = struct
  (* Test receipt of Parse_error due to unrepresentable JSON numbers *)

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
    assert_integer_overflow (fun () -> YAJL.parse (YAJL.make_parser int_callbacks []) json)
    assert_integer_overflow (fun () -> YAJL.parse (YAJL.make_parser int64_callbacks []) json)
    let parser = YAJL.make_parser raw_callbacks []
    YAJL.parse parser json
    assert_equal ~printer:dump
      [Start_map; Map_key "foo"; Number n; End_map] 
      YAJL.complete_parse ~t:List.rev parser

  let below_int64_min_int () = 
    let n = sprintf "%s0" (Int64.to_string Int64.min_int)
    let json = sprintf "{\"foo\": %s}" n
    assert_integer_overflow (fun () -> YAJL.parse (YAJL.make_parser int_callbacks []) json)
    assert_integer_overflow (fun () -> YAJL.parse (YAJL.make_parser int64_callbacks []) json)
    let raw_parser = YAJL.make_parser raw_callbacks []
    YAJL.parse raw_parser json
    assert_equal ~printer:dump
      [Start_map; Map_key "foo"; Number n; End_map] 
      YAJL.complete_parse ~t:List.rev raw_parser

  let above_ocaml_max_int () = 
    let n = Int64.succ (Int64.of_int Pervasives.max_int)
    let ns = Int64.to_string n
    let json = sprintf "{\"foo\": %s}" ns
    assert_integer_overflow (fun () -> YAJL.parse (YAJL.make_parser int_callbacks []) json)
    let int64_parser = YAJL.make_parser int64_callbacks []
    YAJL.parse int64_parser json
    assert_equal ~printer:dump
      [Start_map; Map_key "foo"; Int64 n; End_map] 
      YAJL.complete_parse ~t:List.rev int64_parser
    let raw_parser = YAJL.make_parser raw_callbacks []
    YAJL.parse raw_parser json
    assert_equal ~printer:dump
      [Start_map; Map_key "foo"; Number ns; End_map] 
      YAJL.complete_parse ~t:List.rev raw_parser

  let below_ocaml_min_int () = 
    let n = Int64.pred (Int64.of_int Pervasives.min_int)
    let ns = Int64.to_string n
    let json = sprintf "{\"foo\": %s}" ns
    assert_integer_overflow (fun () -> YAJL.parse (YAJL.make_parser int_callbacks []) json)
    let int64_parser = YAJL.make_parser int64_callbacks []
    YAJL.parse int64_parser json
    assert_equal ~printer:dump
      [Start_map; Map_key "foo"; Int64 n; End_map] 
      YAJL.complete_parse ~t:List.rev int64_parser
    let raw_parser = YAJL.make_parser raw_callbacks []
    YAJL.parse raw_parser json
    assert_equal ~printer:dump
      [Start_map; Map_key "foo"; Number ns; End_map] 
      YAJL.complete_parse ~t:List.rev raw_parser

  let tests = [
    "above Int64.max_int" >:: above_int64_max_int;
    "below Int64.min_int" >:: below_int64_min_int;
    "above ocaml max_int" >:: above_ocaml_max_int;
    "below ocaml min_int" >:: below_ocaml_min_int;
  ]

module ParserOptions = struct
  (* TODO: test setting parser options *)

  let tests = []

let all_tests = ("yajl-ocaml tests" >::: [
    "basic" >::: Basic.tests;
    "multiple buffers" >::: MultiBuffer.tests;
    "raising exceptions in callbacks" >::: CallbackExceptions.tests;
    "state errors" >::: StateErrors.tests;
    "parse errors" >::: ParseErrors.tests;
    "integer overflows" >::: NumberOverflow.tests;
    "parser options" >::: ParserOptions.tests;
])

run_test_tt ~verbose:true all_tests
