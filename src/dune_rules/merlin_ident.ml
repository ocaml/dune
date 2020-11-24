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
