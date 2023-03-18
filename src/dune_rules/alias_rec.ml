open Import

module Alias_status = struct
  module T = struct
    type t =
      | Defined
      | Not_defined

    let empty : t = Not_defined

    let combine : t -> t -> t =
     fun x y ->
      match (x, y) with
      | _, Defined | Defined, _ -> Defined
      | Not_defined, Not_defined -> Not_defined
  end

  include T
  include Monoid.Make (T)
end

module Source_tree_only = struct
  let dep_on_alias_if_exists alias =
    let open Action_builder.O in
    let* definition = Action_builder.of_memo (Load_rules.alias_exists alias) in
    match definition with
    | false -> Action_builder.return Alias_status.Not_defined
    | true ->
      let+ () = Action_builder.alias alias in
      Alias_status.Defined

  module Map_reduce =
    Source_tree.Dir.Make_map_reduce (Action_builder) (Alias_status)

  let dep_on_alias_rec name dir =
    let ctx_name, src_dir = Path.Build.extract_build_context_exn dir in
    let build_dir = Context_name.build_dir (Context_name.of_string ctx_name) in
    let f dir =
      let path =
        Path.Build.append_source build_dir (Source_tree.Dir.path dir)
      in
      dep_on_alias_if_exists (Alias.make ~dir:path name)
    in
    let open Action_builder.O in
    Source_tree.find_dir src_dir |> Action_builder.of_memo >>= function
    | None -> Action_builder.return Alias_status.Not_defined
    | Some src_dir ->
      Map_reduce.map_reduce src_dir ~traverse:Sub_dirs.Status.Set.normal_only ~f
end

module Traverse_build_dirs = struct
  module Lookup_alias = struct
    type result =
      { alias_exists : Alias_status.t
      ; allowed_subdirs : Filename.Set.t
      }

    let of_dir_set ~exists dirs =
      let allowed_subdirs =
        match Dir_set.toplevel_subdirs dirs with
        | Infinite -> Filename.Set.empty
        | Finite sub_dirs -> sub_dirs
      in
      { alias_exists = exists; allowed_subdirs }
  end

  let dep_on_alias_if_exists alias =
    let open Action_builder.O in
    let* load_rules_result =
      Action_builder.of_memo
        (Load_rules.load_dir ~dir:(Path.build (Alias.dir alias)))
    in
    match load_rules_result with
    | Source _ | External _ ->
      Code_error.raise "Alias in a non-build dir"
        [ ("alias", Alias.to_dyn alias) ]
    | Build { aliases; allowed_subdirs; rules_here = _ } -> (
      match Alias.Name.Map.find aliases (Alias.name alias) with
      | None ->
        Action_builder.return
          (Lookup_alias.of_dir_set ~exists:Not_defined allowed_subdirs)
      | Some _ ->
        Action_builder.alias alias
        >>> Action_builder.return
              (Lookup_alias.of_dir_set ~exists:Defined allowed_subdirs))
    | Build_under_directory_target _ ->
      Action_builder.return
        { Lookup_alias.alias_exists = Not_defined
        ; allowed_subdirs = Filename.Set.empty
        }

  let dep_on_alias_rec name dir =
    let rec map_reduce dir ~f =
      let open Action_builder.O in
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
      | false -> Action_builder.return Alias_status.empty
      | true ->
        let* { Lookup_alias.alias_exists; allowed_subdirs } = f dir in
        Action_builder.List.fold_left (String.Set.to_list allowed_subdirs)
          ~init:alias_exists ~f:(fun alias_exists s ->
            let+ alias_exists' = map_reduce (Path.Build.relative dir s) ~f in
            Alias_status.combine alias_exists alias_exists')
    in
    map_reduce dir ~f:(fun dir -> dep_on_alias_if_exists (Alias.make ~dir name))
end

let dep_on_alias_rec ~project alias dir =
  let dune_version = Dune_project.dune_version project in
  match Dune_lang.Syntax.Version.Infix.(dune_version >= (3, 8)) with
  | true -> Traverse_build_dirs.dep_on_alias_rec alias dir
  | false -> Source_tree_only.dep_on_alias_rec alias dir
