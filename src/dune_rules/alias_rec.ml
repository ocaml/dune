open Import

include Action_builder.Alias_rec (struct
  module Map_reduce =
    Source_tree.Dir.Make_map_reduce
      (Action_builder)
      (Action_builder.Alias_status)

  let traverse dir ~f =
    let ctx_name, src_dir = Path.Build.extract_build_context_exn dir in
    let build_dir = Context_name.build_dir (Context_name.of_string ctx_name) in
    let f dir =
      let path =
        Path.Build.append_source build_dir (Source_tree.Dir.path dir)
      in
      f ~path
    in
    let open Action_builder.O in
    Source_tree.find_dir src_dir |> Action_builder.of_memo >>= function
    | None -> Action_builder.return Action_builder.Alias_status.Not_defined
    | Some src_dir ->
      Map_reduce.map_reduce src_dir ~traverse:Sub_dirs.Status.Set.normal_only ~f
end)
