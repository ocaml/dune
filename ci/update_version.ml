open Stdune

(* similar format as dune subst *)
let version = Re.compile (Re.str "%%VERSION%%")

let () =
  let filename = Sys.argv.(1) in
  let filepath = Path.of_filename_relative_to_initial_cwd filename in
  let content = Io.read_file filepath in
  let major, minor = Dune_rpc_private.Version.latest in
  let version_string = sprintf "%d.%d.0" major minor in
  let content = Re.replace_string version ~by:version_string content in
  Printf.printf "# This file is auto-generated. Edit ci/%s instead\n" filename;
  Printf.printf "%s" content
;;
