open! Dune_engine
open! Stdune
open Import

module Dot_merlin = struct
  type directive =
    | EXCLUDE_QUERY_DIR
    | TAG of string * string

  type t = directive list

  let error_tag = "ERROR"

  (* [parse_line l] parses dune-generated .merlin files which only contain a
     subset of merlin configuration options *)
  let parse_line line =
    let line = String.trim line in
    match line with
    | "EXCLUDE_QUERY_DIR" -> Some EXCLUDE_QUERY_DIR
    | line when String.length line = 0 || line.[0] = '#' -> None
    | line -> (
      let open Re in
      let re =
        seq [ group (rep1 upper); rep1 space; group (rep1 notnl) ]
        |> case |> compile
      in
      try
        match Group.all (exec re line) with
        | [| _; tag; value |] -> Some (TAG (tag, value))
        | _ -> raise Not_found
      with Not_found ->
        let msg = Printf.sprintf "Malformed directive \"%s\"" line in
        Some (TAG (error_tag, msg)) )

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

  (* The configuration server will halt on EOF or bad c-sexp *)
  let read_input in_channel =
    match Csexp.input in_channel with
    | Ok sexp -> (
      let open Sexp in
      match sexp with
      | Atom "Halt" -> Halt
      | List [ Atom "File"; Atom path ] -> File path
      | sexp ->
        let msg = Printf.sprintf "Bad input: %s" (Sexp.to_string sexp) in
        Unknown msg )
    | Error _ -> Halt
end

(* [to_local p] makes absolute path [p] relative to the projects root and
   optionally removes the build context *)
let _to_local abs_file_path =
  let error msg = Error msg in
  let path_opt =
    String.drop_prefix
      ~prefix:Path.(to_absolute_filename (of_local Local.root))
      abs_file_path
  in
  match path_opt with
  | Some path -> (
    try
      Ok
        Path.(
          Filename.concat "." path |> of_string |> drop_optional_build_context
          |> local_part)
    with _ -> Printf.sprintf "Could not resolve path %s" path |> error )
  | None ->
    Printf.sprintf "Path is not in dune workspace %s" abs_file_path |> error

let load_merlin_file dir =
  let workspace = Workspace.workspace () in
  let context =
    Option.value ~default:Context_name.default workspace.merlin_context
  in
  let ctx = Context_name.to_string context in
  let dir_path = Path.Build.(append_local (relative root ctx) dir) in
  let file_path = Path.Build.relative dir_path Merlin.merlin_file_name in
  if Path.exists (Path.build file_path) then
    let build = Build.lines_of (Path.build file_path) in
    let lines, _ = Build.exec build in
    Dot_merlin.parse_lines lines
  else
    []

let out s =
  Dot_merlin.to_channel ~out_channel:stdout s;
  flush stdout

let print_merlin_conf file =
  let _dir, _file = Filename.(dirname file, basename file) in
  let answer =
    (* TODO Remove this permanent error when dune stops generating `.merlin`
       files *)
    match
      Error
        "No configuration file found. Try calling `dune build` to generate \
         `.merlin` files."
    with
    | Ok p -> load_merlin_file p
    | Error s -> Dot_merlin.make_error s
  in
  out answer

let start () =
  let rec main () =
    match Commands.read_input stdin with
    | File path ->
      print_merlin_conf path;
      main ()
    | Unknown msg ->
      out (Dot_merlin.make_error msg);
      main ()
    | Halt -> exit 0
  in
  main ()
