(** A high-level JSON representation using convenient data structures from
    {{: https://github.com/ocaml-batteries-team/batteries-included } batteries},
    and {{: https://github.com/mlin/yajl-ocaml } yajl-ocaml} for the parsing and
    stringification.

Note: Although YAJL is very fast, this module's high-level parser, which
produces purely-functional data structures with a lot of generic capabilities,
is not markedly faster than other OCaml JSON libraries (nor slower), and does
not take advantage of YAJL's streaming features. If your main requirement is
super-efficient parsing, you should use the YAJL
{{: http://mlin.github.com/yajl-ocaml/YAJL.html } callback API} directly.
*)

(** {2 JSON representation}

The data structure uses
{{: http://ocaml-batteries-team.github.com/batteries-included/hdoc/BatMap.html } [Batteries.Map]}
for objects and
{{: http://ocaml-batteries-team.github.com/batteries-included/hdoc/BatVect.html } [Batteries.Vect]}
for arrays: *)

(** The general type of JSON data. *)
type t = [
    `Null
  | `Bool of bool
  | `Int of int
  | `Float of float
  | `String of string
  | `Object of (string,t) Batteries_uni.Map.t
  | `Array of t Batteries_uni.Vect.t
]

type json_object = [`Object of (string,t) Batteries_uni.Map.t]
type json_object_or_null = [`Object of (string,t) Batteries_uni.Map.t | `Null]
type json_array = [`Array of t Batteries_uni.Vect.t]
type json_object_or_array = [`Object of (string,t) Batteries_uni.Map.t | `Array of t Batteries_uni.Vect.t]

(** Subsets of [JSON.t] that just serve as abbreviations in the signatures of
operations (below) that return one of these more-specific types. *)
type json_number = [`Int of int | `Float of float]

(** Convenient operators and functions for working with the JSON representation.
    The author tends to [open JSON.Ops] in programs. *)
module Ops : sig
  (** Raised when one of the operators/functions below expects a certain JSON
      type but is given another. Carries the expected type (e.g. ["Object"],
      ["Float"]) and the actual value given. *)
  exception JSON_type_mismatch of string*t

  (** [obj $ "foo"] retrieves the value of key ["foo"] in the object [obj]. *)
  val ($) : t -> string -> t

  (** Raised by the operator [($)] when the given JSON is an object but does not
      contain the expected key. Carries the key and the object. *)
  exception JSON_not_found of string*t

  (** [obj $? "foo"] tests whether object [obj] has key ["foo"]. *)
  val ($?) : t -> string -> bool

  (** [obj $+ ("foo",json)] returns an object in which the key ["foo"] has been
      set to the value [json], replacing a previous value at that key if one was
      present. The original object is not modified. *)
  val ($+) : t -> (string*t) -> json_object

  (** [obj $- "foo"] returns an object in which the key ["foo"] has been removed
      if it existed. No error is raised if there was no such key. The original
      object is not modified. *)
  val ($-) : t -> string -> json_object

  (** [arr $@ k] retrieves the value at zero-based position [k] in the array [arr]. *)
  val ($@) : t -> int -> t

  (** [arr $@! (k,json)] returns an array in which zero-based position [k] has
      been replaced with [json]. The original array is not modified. *)
  val ($@!) : t -> (int*t) -> json_array

  val json_bool : t -> bool
  val json_int : t -> int
  val json_float : t -> float
  val json_float' : t -> float
  val json_number : t -> json_number
  val json_string : t -> string
  val json_object : t -> json_object
  val json_object' : t -> (string,t) Batteries_uni.Map.t
  val json_object_or_null : t -> json_object_or_null
  val json_object_keys : t -> string list
  val json_array : t -> json_array
  val json_array' : t -> t Batteries_uni.Vect.t

  (** Runtime coercions/checkers, any of which may raise [JSON_type_mismatch].

      [json_float'] also accepts [`Int] values and coerces them to [float]. *)
  val json_array_length : t -> int

  val json_of_assoc : (string*t) list -> json_object
  val json_of_list : t list -> json_array

  (** Constructors to ease formulation of JSON using OCaml literal syntax *)
  val json_of_array : t array -> json_array

(** The empty JSON ([JSON.parse "{}"]) *)
val empty : json_object

(** {2 Parsing} *)

(** A subset of the YAJL parser options supported by the high-level parser *)
type parser_option = [
    `Allow_comments
  | `Dont_validate_strings
  | `Allow_trailing_garbage
  (* | `All_numbers_as_floats TODO *)
]

(** Parse a complete JSON stored in memory. The {e unsafe} [pinned] option is
    adopted from [YAJL.parse]; see the
    {{: http://mlin.github.com/yajl-ocaml/YAJL.html } yajl-ocaml docs}
    for an explanation of the {e very specific} conditions under which it can
    be used.
*)
val parse : ?options:(parser_option list) -> ?ofs:int -> ?len:int -> ?pinned:bool -> string -> json_object_or_array

(** Parse a file given the filename; the entire contents are read into memory
    and then parsed. *)
val parse_file : ?options:(parser_option list) -> string -> json_object_or_array

type intermediate

val make_parser : ?options:(parser_option list) -> unit -> intermediate YAJL.parser

(** Expose a YAJL streaming parser to generate [JSON.t]. Maybe convenient if
    you're streaming bytes from a TCP connection. To use the parser, you'd
    call [make_parser] to create one, [YAJL.parse] to add data, and then
    [complete_parse] to retrieve the results. *)
val complete_parse : intermediate YAJL.parser -> json_object_or_array
  

(** {2 Stringification} *)

(** Generate JSON text. Note that YAJL has a hardcoded limitation
of 128 nesting levels for arrays/objects during JSON generation. *)
val to_string : ?options:(YAJL.gen_option list) -> t -> string

(** Write JSON to the given filename. *)
val to_file : ?options:(YAJL.gen_option list) -> string -> t -> unit

(** Low-level: call YAJL generator functions to generate this JSON. *)
val generate : YAJL.gen -> t -> unit
