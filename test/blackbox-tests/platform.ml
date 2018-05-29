open Stdune

type t = Win | Mac

let to_string = function
  | Win -> "win"
  | Mac -> "macosx"

let of_string = function
  | "macosx" -> Mac
  | "win" -> Win
  | s -> invalid_arg ("Platform.of_string: " ^ s)

let arg_name = "-skip-platforms"

let to_cmd = function
  | [] -> []
  | l ->
    [ arg_name
    ; String.concat ~sep:"," (List.map ~f:to_string l)]

let of_args args =
  String.split ~on:',' args
  |> List.map ~f:of_string

let argv v =
  ( arg_name
  , Arg.String (fun s -> v := of_args s)
  , "Comma separated versions of ocaml where to skip test"
  )
