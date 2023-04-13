open Import

(* The implementation of recursive aliases in [dune_rules] differs from that in
   [dune_engine] (see [Action_builder.Alias_rec.dep_on_alias_rec]) because:

     - the engine only traverses directories that exist in the source tree;
     - melange target directories (that don't exist in source) may have
       runtime_deps rules created under them; in this case, dune needs to
       traverse allowed build-only sub-directories. *)

module Alias_status = Action_builder.Alias_status

module Lookup_alias = struct
  type t =
    { alias_status : Alias_status.t
    ; allowed_build_only_subdirs : Filename.Set.t
    }

  let of_dir_set ~status dirs =
    let allowed_build_only_subdirs =
      match Dir_set.toplevel_subdirs dirs with
      | Infinite -> Filename.Set.empty
      | Finite sub_dirs -> sub_dirs
    in
    { alias_status = status; allowed_build_only_subdirs }
end

module In_source_tree = struct
  let dep_on_alias_if_exists alias =
    let open Action_builder.O in
    Action_builder.of_memo
    @@ Load_rules.load_dir ~dir:(Path.build (Alias.dir alias))
    >>= function
    | Source _ | External _ ->
      Code_error.raise "Alias in a non-build dir"
        [ ("alias", Alias.to_dyn alias) ]
    | Build { aliases; allowed_subdirs; rules_here = _ } -> (
      match Alias.Name.Map.find aliases (Alias.name alias) with
      | None ->
        Action_builder.return
          (Lookup_alias.of_dir_set ~status:Not_defined allowed_subdirs)
      | Some _ ->
        Action_builder.alias alias
        >>> Action_builder.return
              (Lookup_alias.of_dir_set ~status:Defined allowed_subdirs))
    | Build_under_directory_target _ ->
      Action_builder.return
      @@ Lookup_alias.of_dir_set ~status:Not_defined Dir_set.empty
end

module In_melange_target_dir = struct
  let dep_on_alias_rec =
    let rec fold dir ~f =
      let open Action_builder.O in
      let* { Lookup_alias.alias_status; allowed_build_only_subdirs } = f dir in
      (* TODO there should be traversals that don't require this conversion *)
      Filename.Set.to_list allowed_build_only_subdirs
      (* TODO: do this in parallel *)
      |> Action_builder.List.fold_left ~init:alias_status
           ~f:(fun alias_status s ->
             fold (Path.Build.relative dir s) ~f
             >>| Action_builder.Alias_status.combine alias_status)
    in
    fun dir ~f:dep_on_alias_if_exists ->
      fold dir ~f:(fun path -> dep_on_alias_if_exists ~path)
end

module Alias_rec (Traverse : sig
  val traverse :
       Path.Build.t
    -> f:(path:Path.Build.t -> Lookup_alias.t Action_builder.t)
    -> Alias_status.t Action_builder.t
end) =
struct
  open Traverse

  let dep_on_alias_rec name dir =
    let f ~path =
      In_source_tree.dep_on_alias_if_exists (Alias.make ~dir:path name)
    in
    traverse dir ~f
end

include Alias_rec (struct
  module Map_reduce =
    Source_tree.Dir.Make_map_reduce
      (Action_builder)
      (Action_builder.Alias_status)

  let traverse dir ~f =
    let open Action_builder.O in
    let ctx_name, src_dir = Path.Build.extract_build_context_exn dir in
    let f =
      let build_dir =
        Context_name.build_dir (Context_name.of_string ctx_name)
      in
      fun dir ->
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
            Action_builder.List.map melange_target_dirs
              ~f:(In_melange_target_dir.dep_on_alias_rec ~f)
          in
          List.fold_left in_melange_target_dirs ~init:found_in_source
            ~f:Alias_status.combine
    in
    Source_tree.find_dir src_dir |> Action_builder.of_memo >>= function
    | None -> Action_builder.return Action_builder.Alias_status.Not_defined
    | Some src_dir ->
      Map_reduce.map_reduce src_dir ~traverse:Sub_dirs.Status.Set.normal_only ~f
end)
