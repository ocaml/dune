open Import

include Action_builder.Alias_rec (struct
  let traverse dir ~f =
    let open Action_builder.O in
    let rec map_reduce dir ~f =
      let* should_traverse =
        match Path.Build.drop_build_context dir with
        | None -> Action_builder.return true
        | Some src_dir -> (
          Action_builder.of_memo (Source_tree.find_dir src_dir) >>| function
          | Some src_dir -> (
            match Source_tree.Dir.status src_dir with
            | Normal -> true
            | Vendored | Data_only -> false)
          | None -> true)
      in
      match should_traverse with
      | false -> Action_builder.return Action_builder.Alias_status.empty
      | true ->
        let* { Action_builder.Lookup_alias.alias_exists; allowed_subdirs } =
          f dir
        in
        Action_builder.List.fold_left (String.Set.to_list allowed_subdirs)
          ~init:alias_exists ~f:(fun alias_exists s ->
            let+ alias_exists' = map_reduce (Path.Build.relative dir s) ~f in
            Action_builder.Alias_status.combine alias_exists alias_exists')
    in
    let f path = f ~path in
    map_reduce dir ~f
end)
