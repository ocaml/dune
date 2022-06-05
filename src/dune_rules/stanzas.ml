open Import

type t =
  { ctx_dir : Path.Build.t
  ; src_dir : Path.Source.t
  ; project : Dune_project.t
  ; stanzas : Stanza.t list
  }

let make_map =
  let make_map_impl build_dir stanzas =
    Path.Build.Map.of_list_map_exn stanzas
      ~f:(fun { Dune_file.dir = src_dir; project; stanzas } ->
        let ctx_dir = Path.Build.append_source build_dir src_dir in
        (ctx_dir, { src_dir; ctx_dir; project; stanzas }))
  in
  let module Input = struct
    type t = Path.Build.t * Dune_file.t list

    let equal = Tuple.T2.equal Path.Build.equal (List.equal Dune_file.equal)

    let hash = Tuple.T2.hash Path.Build.hash (List.hash Dune_file.hash)

    let to_dyn = Dyn.opaque
  end in
  let memo =
    Memo.create "stanzas-dir-map"
      ~input:(module Input)
      (fun (dir, stanzas) -> Memo.return @@ make_map_impl dir stanzas)
  in
  fun ~build_dir stanzas -> Memo.exec memo (build_dir, stanzas)

let in_dir dir =
  let open Memo.O in
  let* (context : Context.t) = Context.DB.by_dir dir in
  let* stanzas = Only_packages.filtered_stanzas context in
  let+ map = make_map ~build_dir:context.build_dir stanzas in
  Path.Build.Map.find map dir
