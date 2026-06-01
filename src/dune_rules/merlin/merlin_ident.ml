open Import

type t =
  | Lib of Lib_name.t * [ `Mode_suffix of Compilation_mode.t | `No_suffix ]
  | Exes of string Nonempty_list.t
  | Melange_entries of string

let for_lib l ~for_ ~mode_suffix =
  Lib (l, if mode_suffix then `Mode_suffix for_ else `No_suffix)
;;

let for_exes ~names = Exes names
let for_melange ~target = Melange_entries target

(* For debug purposes we use the name of one library or executable and the hash
   of the others if there are multiple executables to name the merlin file *)
let to_string = function
  | Lib (name, (`No_suffix | `Mode_suffix Ocaml)) ->
    sprintf "lib-%s" (Lib_name.to_string name)
  | Lib (name, `Mode_suffix Melange) -> sprintf "lib-%s-melange" (Lib_name.to_string name)
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
