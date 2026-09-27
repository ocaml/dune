open Import
open Memo.O

module type Path = sig
  type t

  val diagnostic_name : t -> string
  val relative_to_file : t -> Loc.t -> string -> t
  val equal : t -> t -> bool
  val read : t -> string option Memo.t
end

module Source_file = struct
  type t = Source_tree_file.File.t

  let diagnostic_name = Source_tree_file.File.diagnostic_name
  let relative_to_file = Source_tree_file.File.relative
  let equal = Source_tree_file.File.equal
  let read = Source_tree_file.File.read
end

module Build = struct
  include Path.Build

  let diagnostic_name = to_string
  let relative_to_file t loc f = relative ~error_loc:loc (parent_exn t) f
  let read t = Build_system.read_file (Path.build t) >>| Option.some
end

type 'a context =
  { current_file : 'a
  ; include_stack : (Loc.t * 'a) list
  ; path : (module Path with type t = 'a)
  }

let in_file file path = { current_file = file; include_stack = []; path }
let in_source_file file = in_file file (module Source_file)
let in_src_file file = Source_tree_file.File.workspace file |> in_source_file
let in_build_file file = in_file file (module Build)

let file_path (type a) { path; current_file; _ } loc fn =
  let module Path = (val path : Path with type t = a) in
  Path.relative_to_file current_file loc fn
;;

let error (type a) { current_file = (file : a); include_stack; path } =
  let module Path = (val path : Path with type t = a) in
  let last, rest =
    match include_stack with
    | [] -> assert false
    | last :: rest -> last, rest
  in
  let loc = fst (Option.value (List.last rest) ~default:last) in
  let display_name file = Path.diagnostic_name file |> String.maybe_quoted in
  let line_loc (loc, file) =
    sprintf "%s:%d" (display_name file) (Loc.start loc).pos_lnum
  in
  User_error.raise
    ~loc
    [ Pp.text "Recursive inclusion of dune files detected:"
    ; Pp.textf "File %s is included from %s" (display_name file) (line_loc last)
    ; Pp.chain rest ~f:(fun x -> Pp.textf "included from %s" (line_loc x))
    ]
;;

let load_sexps
      (type a)
      ~context:({ current_file; include_stack; path } as context)
      (loc, fn)
  =
  let module Path = (val path : Path with type t = a) in
  let include_stack = (loc, current_file) :: include_stack in
  let current_file = file_path context loc fn in
  let context = { context with current_file; include_stack } in
  if List.exists include_stack ~f:(fun (_, f) -> Path.equal f current_file)
  then error context;
  let* contents = Path.read current_file in
  let contents =
    match contents with
    | Some contents -> contents
    | None ->
      User_error.raise
        ~loc
        [ Pp.textf
            "File %s doesn't exist."
            (Path.diagnostic_name current_file |> String.maybe_quoted)
        ]
  in
  let lexbuf = Lexbuf.from_string contents ~fname:(Path.diagnostic_name current_file) in
  Memo.return (Dune_lang.Parser.parse ~mode:Many lexbuf, context)
;;
