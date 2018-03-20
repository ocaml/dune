open Import

let path_sep =
  if Sys.win32 then
    ';'
  else
    ':'

let parse_path ?(sep=path_sep) s =
  List.map (String.split s ~on:sep) ~f:Path.absolute

let path =
  match Sys.getenv "PATH" with
  | exception Not_found -> []
  | s -> parse_path s

let exe = if Sys.win32 then ".exe" else ""

let best_prog dir prog =
  let fn = Path.relative dir (prog ^ ".opt" ^ exe) in
  if Path.exists fn then
    Some fn
  else
    let fn = Path.relative dir (prog ^ exe) in
    if Path.exists fn then
      Some fn
    else
      None

let which ?(path=path) prog =
  let rec search = function
    | [] -> None
    | dir :: rest ->
      match best_prog dir prog with
      | None -> search rest
      | Some fn -> Some fn
  in
  search path

let opam = which "opam"

let make =
  match which "gmake" with
  | None -> which "make"
  | some -> some
