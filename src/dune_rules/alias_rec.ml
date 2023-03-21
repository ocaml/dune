open Import
module Alias_status = Action_builder.Alias_status
module Lookup_alias = Action_builder.Lookup_alias

module In_melange_target_dir = struct
  let dep_on_alias_rec dir ~f:dep_on_alias_if_exists =
    let rec map_reduce dir ~f =
      let open Action_builder.O in
      let* { Lookup_alias.alias_status; allowed_build_only_subdirs } = f dir in
      Action_builder.List.fold_left
        (String.Set.to_list allowed_build_only_subdirs) ~init:alias_status
        ~f:(fun alias_status s ->
          let+ alias_status' = map_reduce (Path.Build.relative dir s) ~f in
          Action_builder.Alias_status.combine alias_status alias_status')
    in
    map_reduce dir ~f:(fun path -> dep_on_alias_if_exists ~path)
end

include Action_builder.Alias_rec (struct
  module Map_reduce =
    Source_tree.Dir.Make_map_reduce
      (Action_builder)
      (Action_builder.Alias_status)

  let traverse dir ~f =
    let open Action_builder.O in
    let ctx_name, src_dir = Path.Build.extract_build_context_exn dir in
    let build_dir = Context_name.build_dir (Context_name.of_string ctx_name) in
    let f dir =
      let build_path =
        Path.Build.append_source build_dir (Source_tree.Dir.path dir)
      in
      let* { Lookup_alias.alias_status = found_in_source
           ; allowed_build_only_subdirs = _
           } =
        f ~path:build_path
      and* stanzas_in_dir =
        Action_builder.of_memo (Only_packages.stanzas_in_dir build_path)
      in
      match stanzas_in_dir with
      | None -> Action_builder.return found_in_source
      | Some stanzas ->
        let+ in_melange_target_dirs =
          let melange_target_dirs =
            List.filter_map stanzas.stanzas ~f:(function
              | Melange_stanzas.Emit.T mel ->
                Some (Melange_stanzas.Emit.target_dir ~dir:build_path mel)
              | _ -> None)
          in
          Action_builder.List.map melange_target_dirs ~f:(fun s ->
              In_melange_target_dir.dep_on_alias_rec s ~f)
        in
        List.fold_left in_melange_target_dirs ~init:found_in_source
          ~f:Alias_status.combine
    in
    Source_tree.find_dir src_dir |> Action_builder.of_memo >>= function
    | None -> Action_builder.return Action_builder.Alias_status.Not_defined
    | Some src_dir ->
      Map_reduce.map_reduce src_dir ~traverse:Sub_dirs.Status.Set.normal_only ~f
end)
