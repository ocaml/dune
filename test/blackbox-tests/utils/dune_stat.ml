open Stdune

type data =
  | Hardlinks
  | Permissions

let data_of_string = function
  | "hardlinks" -> Hardlinks
  | "permissions" -> Permissions
  | s -> failwith ("invalid data " ^ s)

let pp_stats data (stats : Unix.stats) =
  match data with
  | Hardlinks -> Int.to_string stats.st_nlink
  | Permissions -> sprintf "%o" stats.st_perm

let () =
  let data = data_of_string Sys.argv.(1) in
  let file = Path.of_filename_relative_to_initial_cwd Sys.argv.(2) in
  let stats = Path.stat file in
  print_endline (pp_stats data stats)
