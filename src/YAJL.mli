(** {1 YAJL callbacks} 

We begin with a direct binding to the YAJL callback-based API, in which the
parser invokes your callbacks for each JSON 'event' and it is mainly up to you
to maintain state and/or build up a data structure based on the sequence of
events. All callbacks are provided with an arbitrary "context value" of type
['a] and return another value of type ['a], which is provided to the next
callback and ultimately returned to the caller of the parser.

Strings are always provided to the callbacks in [string, offset, length] form
where it is only permissible to read the specified range.

Callbacks can cancel parsing by raising any exception (which will be re-raised
to the parser's caller). The parser cannot be used after such an exception.

*)

(** Integers can be parsed either as [int]s or [Int64.t]s. An integer JSON
number outside the representable range will cause the parser to raise
[Parse_error]. *)
type 'a int_callback = [ `Int of ('a -> int -> 'a) | `Int64 of ('a -> Int64.t -> 'a) ]

(** Floating-point callback. A non-integer JSON number that cannot be
represented by a [float] (as determined by the libc [strtod] function) will
cause the parser to raise [Parse_error].*)
type 'a float_callback = 'a -> float -> 'a

(** JSON number callbacks: either a tuple of int and float callbacks, or
request unparsed numbers *)
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

(** Make a new parser with the given callbacks and initial context value. *)
val make_parser : ?options:(parser_options list) -> 'a callbacks -> 'a -> 'a parser

(** [parse parser buf] parses a chunk of data. The first callback is applied
to the initial context value provided to [make_parser], and its return value
is passed to the second callback, and so on. On a second call to [parse] with
more data, the first callback is applied to the last context value returned
from the previous [parse].
@param context provide this value to the first callback to be invoked by this
parse operation, instead of the last-returned value.
@param ofs offset into [buf] at which to begin reading data (default: 0)
@param len amount of data to read (default: [String.length buf - ofs])
 *)
val parse : ?context:'a -> ?ofs:int -> ?len:int -> 'a parser -> string -> unit

(** Complete the parsing of any remaining buffers once there is no more data
to be received, and return the final context value.

@param context as in [parse]
@param t as in [parse]
 *)
val complete_parse : ?context:'a -> ?t:('a -> 'a) -> 'a parser -> 'a

(** May be raised by [parse] or [complete_parse] *)
exception Parse_error of string

(** Retrieve the context value most recently returned by a callback from the
parser (or the initial context value provided to [make_parser], [parse], or
[complete_parse] if no callbacks have executed). This function might be useful
to retrieve an intermediate result in between parsing buffers or following a
[Parse_error] or other exception.

@param t an optional final transformation to apply to the context value before
returning it
*)
val last_context : ?t:('a -> 'a) -> 'a parser -> 'a
