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

type t = [
    `Null
  | `Bool of bool
  | `Int of int
  | `Float of float
  | `String of string
  | `Object of (string,t) Batteries.Map.t
  | `Array of t Batteries.Vect.t
]

(** The empty JSON ([JSON.from_string "{}"] or equivalently [`Object Map.empty]) *)
val empty : t

(** {2 Coercions and operators} *)

(** Raised when one of the operators/functions below expects a certain JSON
    type but is given another. Carries the expected type (e.g. ["Object"],
    ["Float"]) and the actual value given. *)
exception Type_mismatch of string*t

(** Raised when one of the operators/functions below expects a JSON object
    with a certain key, but receives a JSON object without that key. *)
exception No_key of string*t

val bool : t -> bool
val int : t -> int
val float : t -> float
val float' : t -> float
val number : t -> [`Int of int | `Float of float]
val string : t -> string
val obj : t -> (string,t) Batteries.Map.t
val obj_or_null : t -> [`Object of (string,t) Batteries.Map.t | `Null]
val obj_keys : t -> string list
val array : t -> t Batteries.Vect.t

(** Accessors, any of which may raise [JSON.Type_mismatch].

[JSON.float'] also accepts [`Int] values and coerces them to [float]. *)
val array_length : t -> int

val of_assoc : (string*t) list -> t
val of_list : t list -> t

(** Constructors to ease formulating JSON with OCaml syntax literals.
[JSON.of_assoc] constructs an object, while [JSON.of_list] and
[JSON.of_array] construct arrays. *)
val of_array : t array -> t

(** Convenient operators for working with the JSON representation.
    The author tends to [open JSON.Operators] in programs. *)
module Operators : sig

  (** [obj $ "foo"] retrieves the value of key ["foo"] in the object [obj]. *)
  val ($) : t -> string -> t

  (** [obj $? "foo"] tests whether object [obj] has key ["foo"]. *)
  val ($?) : t -> string -> bool

  (** [obj $+ ("foo",json)] returns an object in which the key ["foo"] has been
      set to the value [json], replacing a previous value at that key if one was
      present. The original object is not modified. *)
  val ($+) : t -> (string*t) -> t

  (** [obj $- "foo"] returns an object in which the key ["foo"] has been removed
      if it existed. No error is raised if there was no such key. The original
      object is not modified. *)
  val ($-) : t -> string -> t

  (** [arr $@ k] retrieves the value at zero-based position [k] in the array [arr]. *)
  val ($@) : t -> int -> t

  (** [arr $@! (k,json)] returns an array in which zero-based position [k] has
      been replaced with [json]. The original array is not modified. *)
  val ($@!) : t -> (int*t) -> t

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
val from_string : ?options:(parser_option list) -> ?ofs:int -> ?len:int -> ?pinned:bool -> string -> t

(** Parse a file given the filename; the entire contents are read into memory
    and then parsed. *)
val from_file : ?options:(parser_option list) -> string -> t

type intermediate

val make_parser : ?options:(parser_option list) -> unit -> intermediate YAJL.parser

(** Expose a YAJL streaming parser to generate [JSON.t]. Maybe convenient if
    you're streaming bytes from a TCP connection. To use the parser, you'd
    call [make_parser] to create one, [YAJL.parse] to add data, and then
    [complete_parse] to retrieve the results. *)
val complete_parse : intermediate YAJL.parser -> t
  

(** {2 Stringification} *)

(** Generate JSON text. Note that YAJL has a hardcoded limitation
of 128 nesting levels for arrays/objects during JSON generation. *)
val to_string : ?options:(YAJL.gen_option list) -> t -> string

(** Write JSON to the given filename. *)
val to_file : ?options:(YAJL.gen_option list) -> string -> t -> unit

(** Low-level: call YAJL generator functions to generate this JSON. *)
val generate : YAJL.gen -> t -> unit
