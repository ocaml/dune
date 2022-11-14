open Import

type t =
  | Lib of Lib_name.t
  | Exes of string list
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
    sprintf "exe-%s-%s" name Digest.(generic names |> to_string)
  | Melange_entries name -> sprintf "melange-%s" name
  | Exes [] -> assert false

let merlin_folder_name = ".merlin-conf"

let merlin_file_path path ident =
  Filename.concat merlin_folder_name (to_string ident)
  |> Path.Build.relative path
