open Import

type t =
  | Lib of Lib_name.t
  | Exes of string Nonempty_list.t
  | Melange_entries of string

let for_lib l = Lib l
let for_exes ~names = Exes names
let for_melange ~target = Melange_entries target

(* For debug purposes we use the name of one library or executable and the hash
   of the others if there are multiple executables to name the merlin file *)
let to_string = function
  | Lib name -> sprintf "lib-%s" (Lib_name.to_string name)
  | Exes [ name ] -> sprintf "exe-%s" name
  | Exes (name :: names) ->
    sprintf "exe-%s-%s" name Digest.(repr (Repr.list String.repr) names |> to_string)
  | Melange_entries name -> sprintf "melange-%s" name
;;

let merlin_folder_name = Filename.merlin_conf_dir_basename

let merlin_file_path path ident =
  Filename.concat (Filename.to_string merlin_folder_name) (to_string ident)
  |> Path.Build.relative path
;;
