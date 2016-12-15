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

let rec setup_rec_aliases store (Node (dir, children)) =
  List.map recursive_aliases ~f:(fun make_alias ->
    let alias = make_alias ~dir in
    List.iter children ~f:(fun child ->
      let sub_aliases = setup_rec_aliases store child in
      add_deps store alias sub_aliases);
    alias)

let rules store tree =
  ignore (setup_rec_aliases store tree : t list);
  Hashtbl.fold store ~init:[] ~f:(fun ~key:alias ~data:deps acc ->
    let open Build.O in
    let rule =
      Build.path_set !deps >>>
      Build.create_file ~target:alias (fun _ ->
        close_out (open_out_bin (Path.to_string alias)))
    in
    rule :: acc)
