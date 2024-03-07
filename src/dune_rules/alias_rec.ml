open Import

(* The implementation of recursive aliases in [dune_rules] differs from that in
   [dune_engine] (see [Action_builder.Alias_rec.dep_on_alias_rec]) because:

   - the engine only traverses directories that exist in the source tree;
   - melange target directories (that don't exist in source) may have
     runtime_deps rules created under them; in this case, dune needs to
     traverse allowed build-only sub-directories. *)

module Alias_status = Alias_builder.Alias_status
module Alias_build_info = Alias_builder.Alias_build_info

module In_melange_target_dir = struct
  let dep_on_alias_rec =
    let rec fold dir ~f =
      let open Action_builder.O in
      let* { Alias_build_info.alias_status; allowed_build_only_subdirs } = f dir in
      (* TODO there should be traversals that don't require this conversion *)
      Filename.Set.to_list allowed_build_only_subdirs
      |> Action_builder.List.map ~f:(fun s -> fold (Path.Build.relative dir s) ~f)
      >>| List.fold_left ~init:alias_status ~f:Alias_builder.Alias_status.combine
    in
    fun dir ~f:dep_on_alias_if_exists ->
      fold dir ~f:(fun path -> dep_on_alias_if_exists ~path)
  ;;
end

include Alias_builder.Alias_rec (struct
    module Map_reduce = Source_tree.Dir.Make_map_reduce (Action_builder) (Alias_status)

    let traverse dir ~f =
      let open Action_builder.O in
      let ctx_name, src_dir = Path.Build.extract_build_context_exn dir in
      let f =
        let build_dir = Context_name.build_dir (Context_name.of_string ctx_name) in
        fun dir ->
          let build_path =
            Path.Build.append_source build_dir (Source_tree.Dir.path dir)
          in
          let* { Alias_build_info.alias_status = found_in_source
               ; allowed_build_only_subdirs = _
               }
            =
            f ~path:build_path
          and* stanzas_in_dir =
            Action_builder.of_memo (Dune_load.stanzas_in_dir build_path)
          in
          match stanzas_in_dir with
          | None -> Action_builder.return found_in_source
          | Some stanzas ->
            let+ in_melange_target_dirs =
              let* melange_target_dirs =
                Dune_file.find_stanzas stanzas Melange_stanzas.Emit.key
                |> Action_builder.of_memo
                >>| List.map ~f:(fun mel ->
                  Melange_stanzas.Emit.target_dir ~dir:build_path mel)
              in
              Action_builder.List.map
                melange_target_dirs
                ~f:(In_melange_target_dir.dep_on_alias_rec ~f)
            in
            List.fold_left
              in_melange_target_dirs
              ~init:found_in_source
              ~f:Alias_status.combine
      in
      Source_tree.find_dir src_dir
      |> Action_builder.of_memo
      >>= function
      | None -> Action_builder.return Alias_builder.Alias_status.Not_defined
      | Some src_dir ->
        Map_reduce.map_reduce src_dir ~traverse:Source_dir_status.Set.normal_only ~f
    ;;
  end)
