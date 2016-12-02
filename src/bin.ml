open Import

let path_sep =
  if Sys.win32 then
    ';'
  else
    ':'
;;

let parse_path s =
  let rec loop i j =
    if j = String.length s then
      [Path.absolute (String.sub s ~pos:i ~len:(j - i))]
    else if s.[j] = path_sep then
      Path.absolute (String.sub s ~pos:i ~len:(j - i)) :: loop (j + 1) (j + 1)
    else
      loop i (j + 1)
  in
  loop 0 0
;;

let path =
  match Sys.getenv "PATH" with
  | exception Not_found -> []
  | s -> parse_path s
;;

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
