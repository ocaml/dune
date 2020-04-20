open! Stdune
open Import

module Dot = struct
  type directive =
    | EXCLUDE_QUERY_DIR
    | TAG of string * string

  type t = directive list

  let error_tag = "ERROR"

  let parse_line line =
    let line = String.trim line in
    match line with
    | "EXCLUDE_QUERY_DIR" -> Some EXCLUDE_QUERY_DIR
    | "" -> None
    | line when line.[0] = '#' -> None
    | line ->
      Some
        (let re =
           Re.(
             case (seq [ group (rep1 upper); rep1 space; group (rep1 notnl) ])
             |> compile)
         in
         match Re.Group.all (Re.exec re line) with
         | [| _; tag; value |] -> TAG (tag, value)
         | (exception Not_found)
         | _ ->
           let msg = Printf.sprintf "Malformed directive \"%s\"" line in
           TAG (error_tag, msg))

  let parse_lines lines : t = List.filter_map ~f:parse_line lines

  let directive_to_sexp =
    let open Sexp in
    function
    | EXCLUDE_QUERY_DIR -> List [ Atom "EXCLUDE_QUERY_DIR" ]
    | TAG (tag, value) -> List [ Atom tag; Atom value ]

  let make_error msg = [ TAG (error_tag, msg) ]

  let to_sexp t = Sexp.List (List.map ~f:directive_to_sexp t)

  let to_channel ~out_channel t = Csexp.to_channel out_channel (to_sexp t)
end

module Commands = struct
  type t =
    | File of string
    | Halt
    | Unknown of string

  let read_input in_channel =
    match Csexp.input in_channel with
    | Ok sexp -> (
      let open Sexp in
      match sexp with
      | Atom "Halt" -> Halt
      | List [ Atom "File"; Atom path ] -> File path
      | sexp -> Unknown (Sexp.to_string sexp) )
    | Error _msg -> Halt
end

let to_local abs_file_path =
  let error msg = Error msg in
  let local_root = Path.Local.root in
  let path_opt =
    String.drop_prefix
      ~prefix:(Path.to_absolute_filename (Path.of_local local_root))
      abs_file_path
  in
  match path_opt with
  | Some path -> (
    try
      Ok
        ( path |> Filename.concat "." |> Path.of_string
        |> Path.drop_optional_build_context |> Path.local_part )
    with _ -> Printf.sprintf "Could not parse path %s" path |> error )
  | None ->
    Printf.sprintf "Path is not in dune workspace %s" abs_file_path |> error

let load_merlin_file dir =
  let ctx = Context_name.(to_string default) in
  let dir_path = Path.Build.(append_local (relative root ctx) dir) in
  let file_path = Path.Build.relative dir_path Merlin.merlin_file_name in
  if Path.exists (Path.build file_path) then
    let build = Build.lines_of (Path.build file_path) in
    let lines, _ = Build.exec build in
    Dot.parse_lines lines
  else
    []

let get_merlin_conf file =
  let dir, _file = Filename.(dirname file, basename file) in
  let answer =
    match to_local dir with
    | Ok p -> load_merlin_file p
    | Error s -> Dot.make_error s
  in
  Dot.to_channel ~out_channel:stdout answer;
  Printf.printf "\n%!"

let start () =
  let rec main () =
    match Commands.read_input stdin with
    | File path ->
      get_merlin_conf path;
      main ()
    | Unknown _ -> main ()
    | Halt -> exit 0
  in
  main ()
