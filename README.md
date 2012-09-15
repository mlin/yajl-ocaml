# [yajl-ocaml](https://github.com/mlin/yajl-ocaml)

OCaml bindings for the [YAJL](http://lloyd.github.com/yajl/) streaming JSON
parser. Also includes a convenient, high-level JSON representation.

## Installation

yajl-ocaml works on Linux and Mac OS X with OCaml 3.12 and above.

The package is not yet available in GODI or OASIS-DB. Assuming you have
[findlib](http://projects.camlcity.org/projects/findlib.html) set up, define
the `OCAMLFIND_DESTDIR` environment variable if necessary and

```git clone https://github.com/mlin/yajl-ocaml.git && cd yajl-ocaml && make install```

The Makefile will automatically pull in a few dependencies, including YAJL
itself and [ocaml+twt](http://people.csail.mit.edu/mikelin/ocaml+twt/). You
can then use `ocamlfind` as usual to include the `yajl` package when compiling
your program (making sure `OCAMLPATH` includes `OCAMLFIND_DESTDIR` if you
changed it).

## API documentation

See [ocamldoc:YAJL](http://mlin.github.com/yajl-ocaml/YAJL.html)

## Example usage

### JSON parsing

yajl-ocaml provides fairly direct bindings to the [YAJL callback
API](https://github.com/lloyd/yajl/blob/master/src/api/yajl_parse.h), in which
the parser invokes your callback functions for each JSON "event" and it's
mainly up to you to build a data structure based on the sequence of events.
The bindings help with threading a polymorphic "context value" through your
callbacks.

Here's a program that demonstrates use of the callback API to build up a list
of the events observed during parsing. It then post-processes the list to
print the number of string values in the input.

```ocaml
type event = Null | Bool of bool | Int of int | Float of float | String of string
  | Start_map | Map_key of string | End_map | Start_array | End_array

let on_null lst = Null :: lst
let on_bool lst b = (Bool b) :: lst
let on_int lst i = (Int i) :: lst
let on_float lst x = (Float x) :: lst
let on_string lst buf ofs len = (String (String.sub buf ofs len)) :: lst
let on_start_map lst = Start_map :: lst
let on_map_key lst buf ofs len = (Map_key (String.sub buf ofs len)) :: lst
let on_end_map lst = End_map :: lst
let on_start_array lst = Start_array :: lst
let on_end_array lst = End_array :: lst 

let my_callbacks = {
  YAJL.on_null = on_null;
  on_bool = on_bool;
  on_number =  `Parse_numbers ((`Int on_int), on_float);
  on_string = on_string;
  on_start_map = on_start_map;
  on_map_key = on_map_key;
  on_end_map = on_end_map;
  on_start_array = on_start_array;
  on_end_array = on_end_array
}

let parser = YAJL.make_parser my_callbacks [] ;;
YAJL.parse parser "{\"foo\": [null,false,true,0,12345,3.14159,6e23,\"bar\",\"\"]}" ;;
let events = YAJL.complete_parse parser ;;

Printf.printf "%d strings\n"
  (List.length (List.filter (function String _ -> true | _ -> false) events))
```

### JSON generation

yajl-ocaml also provides bindings to YAJL's JSON generator functions.

```ocaml
let json =
  let gen = YAJL.make_gen () in
  begin
    YAJL.gen_start_map gen;
    YAJL.gen_string gen "key";
    YAJL.gen_start_array gen;
    YAJL.gen_string gen "value";
    List.iter (YAJL.gen_int gen) [1; 2; 3];
    YAJL.gen_end_array gen;
    YAJL.gen_end_map gen;
    let buf, ofs, len = YAJL.gen_get_buf gen in
    String.sub buf ofs len
  end ;;

Printf.printf "%s\n" json
```

This produces: `{"key":["value",1,2,3]}`

## High-level JSON representation

This repo also contains an "extra" OCaml library with a convenient, high-level
JSON representation based on data structures in
[batteries](https://github.com/ocaml-batteries-team/batteries-included). Among
other conveniences, it defines operators to make it especially easy to
manipulate JSON objects (look up keys, set values, etc.).

To use it, `make install-extra` and then use package `JSON`. See
[ocamldoc:JSON](http://mlin.github.com/yajl-ocaml/extra/JSON.html)

As an example, suppose we want to parse this configuration JSON.

```
{"servers": [
  {"host": "10.0.1.17", "port": 9119},
  {"host": "10.0.1.18", "port": 9120}
]}
```

We could do something like this:

```ocaml
open Batteries_uni
open JSON.Operators
let config () =
  let json = JSON.from_file "config.json" in
  let servers = json$"servers" in
  servers |> JSON.array |> Vect.to_array |> Array.map
    (fun entry -> JSON.string (entry$"host"), JSON.int (entry$"port")) ;;

config() |> Array.iter (fun (host,port) -> Printf.printf "%s:%d\n" host port)
```
[batteries](https://github.com/ocaml-batteries-team/batteries-included) is
required for this "extra" library, but not the callback-based yajl-ocaml.

## Performance notes

Given a set of callbacks that don't do anything, yajl-ocaml will go through a
large JSON much faster than other OCaml JSON libraries. Of course, that's not
a fair comparison, because that way of using yajl-ocaml would not produce any
useful results.

Given callbacks that produce a comparable general-purpose data structure, such
as the "extra" JSON library, yajl-ocaml will not be much faster or slower than
other good OCaml JSON libraries like [Yojson](http://mjambon.com/yojson.html),
because that task is mainly bound to memory allocation, number parsing, and
string copying.

yajl-ocaml could be used to its greatest advantage somewhere in between: for
example, suppose you had to process a large numeric matrix represented as JSON
arrays. yajl-ocaml would give you the flexibility to do that much more
efficiently than a general-purpose JSON parser (e.g., streaming the rows one-
by-one while reusing the same `float array` to avoid memory allocation).

## Running tests

Ensure you have [oUnit](http://ounit.forge.ocamlcore.org/) and
[batteries](https://github.com/ocaml-batteries-team/batteries-included)
installed via findlib. Then, just `make test` in the repo directory. This will
automatically wget a certain large, pathological JSON before running the tests
(which use it).
