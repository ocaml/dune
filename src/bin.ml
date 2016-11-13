open Import

let path_sep =
  if Sys.win32 then
    ';'
  else
    ':'
;;

let split_path s =
  let rec loop i j =
    if j = String.length s then
      [String.sub s ~pos:i ~len:(j - i)]
    else if s.[j] = path_sep then
      String.sub s ~pos:i ~len:(j - i) :: loop (j + 1) (j + 1)
    else
      loop i (j + 1)
  in
  loop 0 0
;;

let path =
  match Sys.getenv "PATH" with
  | exception Not_found -> []
  | s -> split_path s
;;

let exe = if Sys.win32 then ".exe" else ""

let best_prog dir prog =
  let fn = dir ^/ prog ^ ".opt" ^ exe in
  if Sys.file_exists fn then
    Some fn
  else
    let fn = dir ^/ prog ^ exe in
    if Sys.file_exists fn then
      Some fn
    else
      None

let find_prog prog =
  let rec search = function
    | [] -> None
    | dir :: rest ->
      match best_prog dir prog with
      | None -> search rest
      | Some fn -> Some (dir, fn)
  in
  search path

let prog_not_found_in_path prog =
  Printf.eprintf "Program %s not found in PATH" prog;
  exit 2

let dir, ocamlc =
  match find_prog "ocamlc" with
  | None -> prog_not_found_in_path "ocamlc"
  | Some x -> x

let prog_not_found prog =
  Printf.eprintf "ocamlc found in %s, but %s/%s doesn't exist" dir dir prog;
  exit 2

let best_prog prog = best_prog dir prog

let get_prog prog =
  match best_prog prog with
  | None -> prog_not_found prog
  | Some fn -> fn

let ocamlopt = best_prog "ocamlopt"
let ocamllex = get_prog "ocamllex"
let ocamldep = get_prog "ocamldep"
