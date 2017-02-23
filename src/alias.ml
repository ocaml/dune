open! Import

type t = Path.t

let make name ~dir =
  Path.relative dir (".jbuild-alias-" ^ name)

let dep = Build.path

let file t = t

let default = make "DEFAULT"
let runtest = make "runtest"

let recursive_aliases =
  [ default
  ; runtest
  ]

module Store = struct
  type nonrec t = (t, Path.Set.t ref) Hashtbl.t

  let create () = Hashtbl.create 1024
end

let add_deps store t deps =
  let deps = Path.Set.of_list deps in
  match Hashtbl.find store t with
  | None -> Hashtbl.add store ~key:t ~data:(ref deps)
  | Some r -> r := Path.Set.union deps !r

type tree = Node of Path.t * tree list

let rec setup_rec_alias store ~make_alias ~prefix ~tree:(Node (dir, children)) =
  let alias = make_alias ~dir:(Path.append prefix dir) in
  add_deps store alias (List.map children ~f:(fun child ->
    setup_rec_alias store ~make_alias ~prefix ~tree:child));
  alias

let setup_rec_aliases store ~prefix ~tree =
  List.iter recursive_aliases ~f:(fun make_alias ->
    ignore (setup_rec_alias store ~make_alias ~prefix ~tree : t))

let rules store ~prefix ~tree =
  setup_rec_aliases store ~prefix ~tree;
  Hashtbl.fold store ~init:[] ~f:(fun ~key:alias ~data:deps acc ->
    let open Build.O in
    let rule =
      Build.path_set !deps >>>
      Build.touch alias
    in
    rule :: acc)
