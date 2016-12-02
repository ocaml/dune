open! Import

type t = Path.t

let make name ~dir =
  Path.relative dir (".jbuild-alias-" ^ name)

let dep = Build_system.Build.path

let file t = t

let default = make "DEFAULT"
let runtest = make "runtest"

let recursive_aliases =
  [ default
  ; runtest
  ]

let db : (t, Path.Set.t ref) Hashtbl.t = Hashtbl.create 1024

let add_deps t deps =
  let deps = Path.Set.of_list deps in
  match Hashtbl.find db t with
  | None -> Hashtbl.add db ~key:t ~data:(ref deps)
  | Some r -> r := Path.Set.union deps !r

type tree = Node of Path.t * tree list

let rec setup_rec_aliases (Node (dir, children)) =
  List.map recursive_aliases ~f:(fun make_alias ->
    let alias = make_alias ~dir in
    List.iter children ~f:(fun child ->
      let sub_aliases = setup_rec_aliases child in
      add_deps alias sub_aliases);
    alias)

let setup_rules tree =
  ignore (setup_rec_aliases tree : t list);
  Hashtbl.iter db ~f:(fun ~key:alias ~data:deps ->
    let open Build_system in
    let open Build.O in
    rule
      (Build.path_set !deps >>>
       Build.create_file ~target:alias (fun _ ->
         close_out (open_out_bin (Path.to_string alias)))))
