open Stdune
module Re = Dune_re

let re =
  Re.compile
    (Re.non_greedy (Re.seq [ Re.str {|--- a/|}; Re.group (Re.rep1 Re.any); Re.eol ]))
;;

let files_of_patch ~dir patch =
  Re.all re patch
  |> List.map ~f:(fun group ->
    let group = Re.Group.get group 1 in
    let local_path = Path.Local.of_string group in
    Path.append_local dir local_path)
;;

let files_of_patch_file ~dir path = Io.read_file path |> files_of_patch ~dir
