(** {1 YAJL callbacks} 

We begin with a direct binding to the YAJL callback-based API, in which the parser invokes your
callbacks for each JSON 'event' and it is mainly up to you to maintain state and/or build up a
data structure based on the sequence of events. All callbacks are provided with an arbitrary context
value of type ['a] and return another value of type ['a], which is provided to the next callback
and ultimately returned to the caller of the parser. Strings are always provided to the callbacks
in [string, offset, length] form where it is only permissible to read the specified range.
Callbacks can cancel parsing by raising any exception (which will be re-raised to the parser's
caller).

*)

(** Integers can be parsed either as [int]s or [Int64.t]s. An integer JSON number outside the
representable range will cause the parser to raise [Parse_error]. *)
type 'a int_callback = [ `Int of ('a -> int -> 'a) | `Int64 of ('a -> Int64.t -> 'a) ]

(** Floating-point callback. A non-integer JSON number that cannot be represented by a [float] (as
determined by the libc [strtod] function) will cause the parser to raise [Parse_error].*)
type 'a float_callback = 'a -> float -> 'a

(** JSON number callbacks: either a tuple of int and float callbacks, or request unparsed numbers *)
type 'a number_callbacks = [
	  `Parse_numbers of (('a int_callback)*('a float_callback))
	| `Raw_numbers of ('a -> string -> int -> int -> 'a)
]

(** YAJL callbacks *)
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

(** {1 Callback parser} *)

type 'a parser

type parser_options = [
	  `Allow_comments
	| `Dont_validate_strings
	| `Allow_trailing_garbage
	| `Allow_multiple_values
	| `Allow_partial_values
]

(** Make a new parser with the given options and callbacks. *)
val make_parser : ?options:(parser_options list) -> 'a callbacks -> 'a parser

(** [parse parser ctx buf ofs len] parses a chunk of data. The first callback is applied to [ctx],
and its return value is passed to the second callback, and so on, until the return value of the last
callback is returned to the caller. If there are further chunks of data to parse, you would
typically pass the return value of the previous call to [parse] as the context for the next call. *)
val parse : ?ofs:int -> ?len:int -> 'a parser -> 'a -> string -> 'a

(** Complete the parsing of any remaining buffers once there is no more data to be received. *)
val complete_parse : 'a parser -> 'a -> 'a

(** May be raised by [parse] or [complete_parse] *)
exception Parse_error of string
