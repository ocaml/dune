open Import

module type Path = sig
  type t

  val parent_exn : t -> t
  val to_string_maybe_quoted : t -> string
  val relative : t -> Loc.t -> Filename.t -> t
  val equal : t -> t -> bool
  val file_exists : t -> bool Memo.t
  val with_lexbuf_from_file : t -> f:(Lexing.lexbuf -> 'a) -> 'a Memo.t
end

module Source = struct
  include Path.Source

  let relative t loc f = relative ~error_loc:loc t f
  let file_exists t = Fs_memo.file_exists (In_source_dir t)
  let with_lexbuf_from_file t ~f = Fs_memo.with_lexbuf_from_file (In_source_dir t) ~f
end

module Build = struct
  include Path.Build

  let relative t loc f = relative ~error_loc:loc t f
  let file_exists _ = Memo.return true

  let with_lexbuf_from_file t ~f =
    Build_system.with_file (Path.build t) ~f:(fun path ->
      Io.Untracked.with_lexbuf_from_file path ~f)
  ;;
end

type 'a context =
  { current_file : 'a
  ; include_stack : (Loc.t * 'a) list
  ; path : (module Path with type t = 'a)
  }

let in_file file path = { current_file = file; include_stack = []; path }
let in_src_file file = in_file file (module Source)
let in_build_file file = in_file file (module Build)

let file_path (type a) { path; current_file; _ } loc fn =
  let module Path = (val path : Path with type t = a) in
  let dir = Path.parent_exn current_file in
  Path.relative dir loc fn
;;

let error (type a) { current_file = (file : a); include_stack; path } =
  let module Path = (val path : Path with type t = a) in
  let last, rest =
    match include_stack with
    | [] -> assert false
    | last :: rest -> last, rest
  in
  let loc = fst (Option.value (List.last rest) ~default:last) in
  let line_loc (loc, file) =
    sprintf "%s:%d" (Path.to_string_maybe_quoted file) (Loc.start loc).pos_lnum
  in
  User_error.raise
    ~loc
    [ Pp.text "Recursive inclusion of dune files detected:"
    ; Pp.textf
        "File %s is included from %s"
        (Path.to_string_maybe_quoted file)
        (line_loc last)
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
  let open Memo.O in
  let* exists = Path.file_exists current_file in
  if not exists
  then
    User_error.raise
      ~loc
      [ Pp.textf "File %s doesn't exist." (Path.to_string_maybe_quoted current_file) ];
  let context = { context with current_file; include_stack } in
  if List.exists include_stack ~f:(fun (_, f) -> Path.equal f current_file)
  then error context;
  let+ sexps =
    Path.with_lexbuf_from_file current_file ~f:(Dune_lang.Parser.parse ~mode:Many)
  in
  sexps, context
;;
