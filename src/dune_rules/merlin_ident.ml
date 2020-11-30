open Dune_engine
open Import

type t =
  | Lib of Lib_name.t
  | Exes of string list

let for_lib l = Lib l

let for_exes ~names = Exes names

let to_string = function
  | Lib name -> sprintf "lib-%s" (Lib_name.to_string name)
  | Exes names -> sprintf "exe-%s" (String.concat ~sep:"-" names)

let merlin_exist_name = ".merlin-exist"

let merlin_folder_name = ".merlin-conf"

let merlin_exists_path path ident =
  String.concat ~sep:"-" [ merlin_exist_name; to_string ident ]
  |> Path.Build.relative path

let merlin_file_path path ident =
  Filename.concat merlin_folder_name (to_string ident)
  |> Path.Build.relative path
