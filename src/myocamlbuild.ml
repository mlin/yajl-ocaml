open Ocamlbuild_plugin
open Command

open Printf 

let yajl_prefix = (try Sys.getenv "YAJL_PREFIX" with Not_found -> "/usr")

;;

dispatch begin function
  | After_rules ->
    flag ["use_yajl"; "c"; "compile"] & S[
      A"-ccopt"; A (sprintf "-I%s/include" yajl_prefix)
    ];

    flag ["use_yajl"; "c"; "ocamlmklib"] & S[
      A (sprintf "-L%s/lib" yajl_prefix); A "-lyajl_s"; A"-custom"
    ];

    dep ["use_yajl"; "ocaml"; "library"; "link"] ["libyajl_stubs.a"];

    flag ["use_yajl"; "ocaml"; "library"; "link"; "byte"] & S[
      A"-custom"; A"-cclib"; A"-lyajl_stubs"; A"-cclib"; A "-lyajl_s"
    ];

    flag ["use_yajl"; "ocaml"; "library"; "link"; "native"] & S[
      A"-cclib"; A"-lyajl_stubs"; A"-cclib"; A "-lyajl_s"
    ]
  | _ -> ()
end
