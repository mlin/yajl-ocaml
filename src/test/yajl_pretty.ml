open Printf

if Unix.isatty Unix.stdin && Array.length Sys.argv < 2 then
  eprintf "Usage: yajl_pretty [file.json]\nAlso accepts piped input.\n"
  exit 1

let callbacks = {
  YAJL.on_null = (fun gen -> YAJL.gen_null gen; gen);
  on_bool = (fun gen b -> YAJL.gen_bool gen b; gen);
  on_number = `Raw_numbers (fun gen buf ofs len -> YAJL.gen_number gen buf ~ofs ~len; gen);
  on_string = (fun gen buf ofs len -> YAJL.gen_string gen buf ~ofs ~len; gen);
  on_start_map = (fun gen -> YAJL.gen_start_map gen; gen);
  on_end_map = (fun gen -> YAJL.gen_end_map gen; gen);
  on_map_key = (fun gen buf ofs len -> YAJL.gen_string gen buf ~ofs ~len; gen);
  on_start_array = (fun gen -> YAJL.gen_start_array gen; gen);
  on_end_array = (fun gen -> YAJL.gen_end_array gen; gen)
}

let gen = YAJL.make_gen ~options:[`Beautify_with "  "] ()
let parser = YAJL.make_parser ~options:[`Allow_multiple_values] callbacks gen
let bufsz = 8192
let buf = String.create bufsz

let g () =
  let (genbuf,genofs,genlen) = YAJL.gen_get_buf gen
  output stdout genbuf genofs genlen
  YAJL.gen_clear gen

let f chan =
  try
    while true do
      let n = input chan buf 0 bufsz
      if n = 0 then raise Exit
      YAJL.parse parser buf ~len:n
      g ()
  with
    | Exit -> g ()

if Array.length Sys.argv <= 1 then
  f stdin
else
  for i = 1 to Array.length Sys.argv - 1 do
    let chan = open_in_bin Sys.argv.(i)
    f chan
    close_in chan
