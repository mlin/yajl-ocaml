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
      YAJL.context ~t:List.rev parser

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
      YAJL.context ~t:List.rev parser

    YAJL.parse ~context:[String "foo"] parser json_half2

    assert_equal ~printer:dump
      [String "foo"; String "dolor"; String "sit"; String "amet"; End_array; End_map]
      YAJL.context ~t:List.rev parser

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
      YAJL.context ~t:List.rev parser

    YAJL.parse parser ("0123456789" ^ json_half2 ^ "0123456789") ~ofs:10 ~len:(String.length json_half2)

    assert_equal ~printer:dump
      [Start_map; Map_key "foo"; Start_array;
        String "Lorem"; String "ipsum"; String "dolor"; String "sit"; String "amet";
       End_array; End_map]
      YAJL.complete_parse ~t:List.rev parser

  let big_json () =
    if Array.length Sys.argv <= 1 then printf "(skipping since a JSON filename was not passed on the command line)\n"
    else
      File.with_file_in Sys.argv.(1)
        fun infile ->
          let parser = YAJL.make_parser raw_callbacks []
          let bytes = ref 0
          try
            let buf = String.create 8192
            while true do
              let n = IO.input infile buf 0 8192
              YAJL.parse parser buf ~len:n
              bytes := !bytes + n
          with IO.No_more_input -> ()
          IO.close_in infile
          assert_equal ~printer:string_of_int 5276 (List.length (YAJL.complete_parse parser))

  let tests = [
    "two halves" >:: halves;
    "override context value" >:: override_context_value;
    "partial buffers" >:: partial_buffers;
    "big JSON" >:: big_json
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
      YAJL.context ~t:List.rev parser

  let tests = [
    "trivial" >:: trivial;
    "interruption" >:: interruption
  ]

module ParseErrors = struct
  let assert_parse_error f =
    try
      ignore (f ())
      "no exception raised" @? false
    with
      | YAJL.Parse_error _ -> ()
      | exn -> raise exn

  let trivial () =
    assert_parse_error (fun () -> YAJL.parse (YAJL.make_parser raw_callbacks []) "}")
    let parser = YAJL.make_parser raw_callbacks []
    YAJL.parse parser "{\"foo\": 12345, \"ba"
    assert_parse_error (fun () -> YAJL.complete_parse parser)

  let tests = [
    "trivial" >:: trivial
  ]

module IntRepr = struct
  (* test parsing of borderline integers *)

  let borderline_ints = [ 0L; 1L;
                          1073741823L; 1073741824L; 2147483647L; 2147483648L;
                          4611686018427387903L; 4611686018427387904L; 9223372036854775807L;
                          -1L;
                          -1073741824L; -1073741825L; -2147483648L; -2147483649L;
                          -4611686018427387904L; -4611686018427387905L; -9223372036854775807L ]
  (* Int64.min_int is actually -9223372036854775808L, but YAJL won't parse that:
     https://github.com/lloyd/yajl/blob/8b48967c74e9b16c07f120b71598f5e5269e8f57/src/yajl_parser.c#L36 *)

  let int () =
    borderline_ints |> List.iter
      fun iL ->
        if Int64.of_int min_int < iL && iL < Int64.of_int max_int then
          let i = Int64.to_int iL
          let json = sprintf "{\"foo\": %d}" i
          let p = YAJL.make_parser int_callbacks []
          YAJL.parse p json
          assert_equal ~printer:dump
            [Start_map; Map_key "foo"; Int i; End_map]
            YAJL.complete_parse ~t:List.rev p

  let int64 () =
    borderline_ints |> List.iter
      fun iL ->
        let json = sprintf "{\"foo\": %Ld}" iL
        let p = YAJL.make_parser int64_callbacks []
        YAJL.parse p json
        assert_equal ~printer:dump
          [Start_map; Map_key "foo"; Int64 iL; End_map]
          YAJL.complete_parse ~t:List.rev p

  let raw () =
    borderline_ints |> List.iter
      fun iL ->
        let json = sprintf "{\"foo\": %Ld}" iL
        let p = YAJL.make_parser raw_callbacks []
        YAJL.parse p json
        assert_equal ~printer:dump
          [Start_map; Map_key "foo"; Number (Int64.to_string iL); End_map]
          YAJL.complete_parse ~t:List.rev p

  let tests = [
    "int" >:: int;
    "int64" >:: int64;
    "raw" >:: raw
  ]

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
  (* Test setting parser options *)

  let assert_parse_error f =
    try
      ignore (f ())
      "no exception raised" @? false
    with
      | YAJL.Parse_error _ -> ()
      | exn -> raise exn

  let both_parsers opts =
    let parser_off = YAJL.make_parser raw_callbacks []
    let parser_on = YAJL.make_parser ~options:opts raw_callbacks []
    parser_off, parser_on

  let allow_comments () =
    let json = "{\"foo\": \"bar\", /* comment */ \"baz\": 12345}"
    let parser_off, parser_on = both_parsers [`Allow_comments]
    assert_parse_error (fun () -> YAJL.parse parser_off json)
    YAJL.parse parser_on json
    assert_equal ~printer:dump
      [Start_map; Map_key "foo"; String "bar"; Map_key "baz"; Number "12345"; End_map] 
      YAJL.complete_parse ~t:List.rev parser_on

  let dont_validate_strings () =
    let json = "{\"foo\": \"\xc3\x28\"}"
    let parser_off, parser_on = both_parsers [`Dont_validate_strings]
    assert_parse_error (fun () -> YAJL.parse parser_off json)
    YAJL.parse parser_on json
    assert_equal ~printer:dump
      [Start_map; Map_key "foo"; String "\xc3\x28"; End_map] 
      YAJL.complete_parse ~t:List.rev parser_on

  let allow_trailing_garbage () =
    let json = "{}garbage"
    let parser_off, parser_on = both_parsers [`Allow_trailing_garbage]
    assert_parse_error (fun () -> YAJL.parse parser_off json)
    YAJL.parse parser_on json
    assert_equal ~printer:dump
      [Start_map; End_map] 
      YAJL.complete_parse ~t:List.rev parser_on

  let allow_multiple_values () =
    let json = "{\"foo\": \"bar\"}{\"baz\": 12345}"
    let parser_off, parser_on = both_parsers [`Allow_multiple_values]
    assert_parse_error (fun () -> YAJL.parse parser_off json)
    YAJL.parse parser_on json
    assert_equal ~printer:dump
      [Start_map; Map_key "foo"; String "bar"; End_map; Start_map; Map_key "baz"; Number "12345"; End_map] 
      YAJL.complete_parse ~t:List.rev parser_on

  let allow_partial_values () =
    let json = "{\"foo\": \"bar\", \"baz\": "
    let parser_off, parser_on = both_parsers [`Allow_partial_values]
    assert_parse_error (fun () -> YAJL.parse parser_off json; YAJL.complete_parse parser_off)
    YAJL.parse parser_on json
    assert_equal ~printer:dump
      [Start_map; Map_key "foo"; String "bar"; Map_key "baz"] 
      YAJL.complete_parse ~t:List.rev parser_on

  let altogether () =
    let json = "{\"foo\": \"bar\"}/*comment*/{\"\xc3\x28\": \"\xf0\x90\x28\xbc\""
    let parser_off, parser_on = both_parsers [`Allow_comments; `Dont_validate_strings; `Allow_multiple_values; `Allow_partial_values]
    assert_parse_error (fun () -> YAJL.parse parser_off json; YAJL.complete_parse parser_off)
    YAJL.parse parser_on json
    assert_equal ~printer:dump
      [Start_map; Map_key "foo"; String "bar"; End_map; Start_map; Map_key "\xc3\x28"; String "\xf0\x90\x28\xbc"]
      YAJL.complete_parse ~t:List.rev parser_on

  let tests = [
    "allow comments" >:: allow_comments;
    "don't validate strings" >:: dont_validate_strings;
    "allow trailing garbage" >:: allow_trailing_garbage;
    "allow multiple values" >:: allow_multiple_values;
    "allow partial values" >:: allow_partial_values;
    "altogether" >:: altogether
  ]

module PinnedBuffers = struct
  (* TODO: test calling parse with ~pinned:true *)

  let tests = []

module ParserGC = struct
  (* TODO: test garbage-collection of parser *)

  let tests = []

module Gen = struct
  let gen yajl = function
    | Null -> YAJL.gen_null yajl
    | Bool b -> YAJL.gen_bool yajl b
    | Int n -> YAJL.gen_int yajl n
    | Int64 n -> YAJL.gen_int64 yajl n
    | Float x -> YAJL.gen_float yajl x
    | Number buf -> YAJL.gen_number yajl buf
    | String buf -> YAJL.gen_string yajl buf
    | Start_map -> YAJL.gen_start_map yajl
    | Map_key buf -> YAJL.gen_string yajl buf
    | End_map -> YAJL.gen_end_map yajl
    | Start_array -> YAJL.gen_start_array yajl
    | End_array -> YAJL.gen_end_array yajl

  let test_gen ?options ?parser_options which_callbacks json =
    let events =
      let parser = YAJL.make_parser ?options:parser_options which_callbacks []
      YAJL.parse parser json
      List.rev (YAJL.complete_parse parser)

    let gen_json =
      let yajl = YAJL.make_gen ?options ()
      List.iter (gen yajl) events
      let (buf,ofs,len) = YAJL.gen_get_buf yajl
      String.sub buf ofs len

    (*printf "%s\n" gen_json*)

    let events2 =
      let parser2 = YAJL.make_parser ?options:parser_options which_callbacks []
      YAJL.parse parser2 gen_json
      List.rev (YAJL.complete_parse parser2)

    assert_equal ~printer:dump events events2

  let basic_json = "{\"foo\": [null,false,true,0,12345,3.14159,6e23,\"bar\",\"\"]}"
  let basic_int () = test_gen int_callbacks basic_json
  let basic_int64 () = test_gen int64_callbacks basic_json
  let basic_raw () = test_gen raw_callbacks basic_json

  let beautify_default () = test_gen ~options:[`Beautify] int_callbacks basic_json
  let beautify_custom () = test_gen ~options:[`Beautify_with "                 "] int_callbacks basic_json

  let validate_utf8 () =
    let bogus = "{\"foo\": \"ok\xc3\x28\"}"
    test_gen ~parser_options:[`Dont_validate_strings] int_callbacks bogus
    try
      test_gen ~options:[`Validate_UTF8] ~parser_options:[`Dont_validate_strings] int_callbacks bogus
      assert false
    with
      | YAJL.Gen_invalid_string (buf,ofs,len) ->
        (* printf "%s %d %d\n" buf ofs len *)
        assert_equal "ok\xc3\x28" (String.sub buf ofs len)
      | _ -> assert false

  let int_repr () =
    IntRepr.borderline_ints |> List.iter
      fun iL ->
        if Int64.of_int min_int < iL && iL < Int64.of_int max_int then
          let i = Int64.to_int iL
          test_gen int_callbacks (sprintf "{\"foo\": %d}" i)
        let json = (sprintf "{\"foo\": %Ld}" iL)
        test_gen int64_callbacks json
        test_gen raw_callbacks json

  let invalid_float () =
    let yajl = YAJL.make_gen ()
    gen yajl Start_map; gen yajl (Map_key "string")
    assert_raises (YAJL.Gen_invalid_float infinity) (fun () -> gen yajl (Float infinity))

  let tests = [
    "basic (int)" >:: basic_int;
    "basic (int64)" >:: basic_int64;
    "basic (raw)" >:: basic_raw;
    "Beautify" >:: beautify_default;
    "Beautify (custom indentation)" >:: beautify_custom;
    "Validate_UTF8" >:: validate_utf8;
    "int representation" >:: int_repr;
    "Invalid float" >:: invalid_float;
  ]

(*
FIXME: Streaming integration test is disabled because the big sample JSON
actually exceeds the YAJL generator's maximum array nesting level. To run the
test, change YAJL_MAX_DEPTH in ../../upstream/src/api/yajl_common.h to 512 and
uncomment the appropriate line in all_tests, below.
*)
let streaming_integration_test () =
    if Array.length Sys.argv <= 1 then printf "(skipping since a JSON filename was not passed on the command line)\n"
    else
      File.with_file_in Sys.argv.(1)
        fun infile ->
          printf "here0\n"; flush stdout
          (* get the big JSON as an event list *)
          let json = IO.read_all infile
          IO.close_in infile
          let parser = YAJL.make_parser raw_callbacks []
          YAJL.parse parser json
          let events = YAJL.complete_parse ~t:List.rev parser

          printf "here1\n"; flush stdout
          (* regenerate the JSON in a streaming manner (100 events at a time) *)
          let yajl = YAJL.make_gen ()
          let rec stream_json events bufs =
            if events = [] then List.rev(bufs)
            else
              let batch, rest = List.split_at (min 100 (List.length events)) events
              List.iter (Gen.gen yajl) batch
              let (buf,ofs,len) = YAJL.gen_get_buf yajl
              let bufcpy = String.sub buf ofs len
              YAJL.gen_clear yajl
              stream_json rest (bufcpy :: bufs)
          let gen_bufs = stream_json events []

          printf "here2\n"; flush stdout
          (* stream the generated buffers back through a parser *)
          let parser2 = YAJL.make_parser raw_callbacks []
          gen_bufs |> List.iter (YAJL.parse parser2)

          (* confirm we get the same event list back *)
          assert_equal events (YAJL.complete_parse ~t:List.rev parser2)

let all_tests = ("yajl-ocaml tests" >::: [
    "basic" >::: Basic.tests;
    "multiple buffers" >::: MultiBuffer.tests;
    "raising exceptions in callbacks" >::: CallbackExceptions.tests;
    "parse errors" >::: ParseErrors.tests;
    "integer representation" >::: IntRepr.tests;
    "integer overflows" >::: NumberOverflow.tests;
    "parser options" >::: ParserOptions.tests;
    "pinned buffers" >::: PinnedBuffers.tests;
    "parser garbage collection" >::: ParserGC.tests;
    "generator tests" >::: Gen.tests;
(*    "streaming integration test" >:: streaming_integration_test *)
])

run_test_tt ~verbose:true all_tests
