open! Import

module Name : sig
  type t
  val make : Path.t -> t
  val path : t -> Path.t
end = struct
  type t = Path.t
  let make t = t
  let path t = t
end

type t =
  { name : Name.t
  ; file : Path.t
  }

let aliases_path = Path.(relative root) "_build/.aliases"

let suffix = "-" ^ String.make 32 '0'

let of_path path =
  if not (Path.is_local path) then
    die "Aliases are only supported for local paths!\n\
         Tried to reference alias %S"
      (Path.to_string path);
  { name = Name.make path
  ; file = Path.extend_basename (Path.append aliases_path path) ~suffix
  }

let make name ~dir = of_path (Path.relative dir name)

let dep t = Build.path t.file

let file t = t.file

let file_with_digest_suffix t ~digest =
  let dir = Path.parent t.file in
  let base = Path.basename t.file in
  let len = String.length base in
  Path.relative dir
    (String.sub base ~pos:0 ~len:(len - 32) ^ Digest.to_hex digest)

let default = make "DEFAULT"
let runtest = make "runtest"
let install = make "install"

let lib_cm_all ~dir lib_name cm_kind =
  make (sprintf "%s%s-all" lib_name (Cm_kind.ext cm_kind)) ~dir

let recursive_aliases =
  [ default
  ; runtest
  ; install
  ]

module Store = struct
  type entry =
    { alias : t
    ; mutable deps : Path.Set.t
    }
  type t = (Name.t, entry) Hashtbl.t

  let create () = Hashtbl.create 1024
end

let add_deps store t deps =
  let deps = Path.Set.of_list deps in
  match Hashtbl.find store t.name with
  | None ->
    Hashtbl.add store ~key:t.name
      ~data:{ Store.alias = t
            ; deps = deps
            }
  | Some e -> e.deps <- Path.Set.union deps e.deps

type tree = Node of Path.t * tree list

let rec setup_rec_alias store ~make_alias ~prefix ~tree:(Node (dir, children)) =
  let alias = make_alias ~dir:(Path.append prefix dir) in
  add_deps store alias (List.map children ~f:(fun child ->
    setup_rec_alias store ~make_alias ~prefix ~tree:child));
  alias.file

let setup_rec_aliases store ~prefix ~tree =
  List.iter recursive_aliases ~f:(fun make_alias ->
    ignore (setup_rec_alias store ~make_alias ~prefix ~tree : Path.t))

let rules store ~prefixes ~tree =
  List.iter prefixes ~f:(fun prefix ->
    setup_rec_aliases store ~prefix ~tree);

  (* For each alias @_build/blah/../x, add a dependency: @../x --> @_build/blah/../x *)
  Hashtbl.fold store ~init:[] ~f:(fun ~key:_ ~data:{ Store. alias; _ } acc ->
    match Path.extract_build_context (Name.path alias.name) with
    | None -> acc
    | Some (_, in_src) -> (of_path in_src, alias) :: acc)
  |> List.iter ~f:(fun (in_src, in_build_dir) ->
      add_deps store in_src [in_build_dir.file]);

  Hashtbl.fold store ~init:[] ~f:(fun ~key:_ ~data:{ Store. alias; deps } acc ->
    let open Build.O in
    let rule =
      Build_interpret.Rule.make
        (Build.path_set deps >>>
         Build.create_file alias.file)
    in
    rule :: acc)
