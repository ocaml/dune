open! Dune_engine
open Stdune
open Action_builder.O
open Dep_conf

let make_alias expander s =
  let loc = String_with_vars.loc s in
  Expander.expand_path expander s >>| Alias.of_user_written_path ~loc

module Source_tree_map_reduce =
  Source_tree.Dir.Make_map_reduce (Action_builder) (Monoid.Union (Path.Set))

let collect_source_files_recursively dir ~f =
  let prefix_with, dir = Path.extract_build_context_dir_exn dir in
  Action_builder.memo_build (Source_tree.find_dir dir) >>= function
  | None -> Action_builder.return Path.Set.empty
  | Some dir ->
    Source_tree_map_reduce.map_reduce dir ~traverse:Sub_dirs.Status.Set.all
      ~f:(fun dir ->
        f (Path.append_source prefix_with (Source_tree.Dir.path dir)))

let dep expander = function
  | File s ->
    let* path = Expander.expand_path expander s in
    let+ () = Action_builder.path path in
    [ path ]
  | Alias s ->
    let* a = make_alias expander s in
    let+ () = Action_builder.alias a in
    []
  | Alias_rec s ->
    let* a = make_alias expander s in
    let+ () = Build_system.Alias.dep_rec ~loc:(String_with_vars.loc s) a in
    []
  | Glob_files { glob = s; recursive } ->
    let loc = String_with_vars.loc s in
    let* path = Expander.expand_path expander s in
    let pred = Glob.of_string_exn loc (Path.basename path) |> Glob.to_pred in
    let dir = Path.parent_exn path in
    let files_in dir =
      Action_builder.paths_matching ~loc (File_selector.create ~dir pred)
    in
    let+ files =
      if recursive then
        collect_source_files_recursively dir ~f:files_in
      else
        files_in dir
    in
    Path.Set.to_list files
  | Source_tree s ->
    let* path = Expander.expand_path expander s in
    Action_builder.map ~f:Path.Set.to_list
      (Action_builder.source_tree ~dir:path)
  | Package p ->
    let* pkg = Expander.expand_str expander p in
    let+ () =
      let pkg = Package.Name.of_string pkg in
      let context = Expander.context expander in
      match Expander.find_package expander pkg with
      | Some (Local pkg) ->
        Action_builder.alias
          (Build_system.Alias.package_install
             ~context:(Context.build_context context)
             ~pkg)
      | Some (Installed pkg) ->
        let version =
          Dune_project.dune_version @@ Scope.project @@ Expander.scope expander
        in
        if version < (2, 9) then
          Action_builder.fail
            { fail =
                (fun () ->
                  let loc = String_with_vars.loc p in
                  User_error.raise ~loc
                    [ Pp.textf
                        "Dependency on an installed package requires at least \
                         (lang dune 2.9)"
                    ])
            }
        else
          let files =
            List.concat_map
              ~f:(fun (s, l) ->
                let dir = Section.Map.find_exn pkg.sections s in
                List.map l ~f:(fun d ->
                    Path.relative dir (Install.Dst.to_string d)))
              pkg.files
          in
          Action_builder.paths files
      | None ->
        Action_builder.fail
          { fail =
              (fun () ->
                let loc = String_with_vars.loc p in
                User_error.raise ~loc
                  [ Pp.textf "Package %s does not exist"
                      (Package.Name.to_string pkg)
                  ])
          }
    in
    []
  | Universe ->
    let+ () = Action_builder.dep Dep.universe in
    []
  | Env_var var_sw ->
    let* var = Expander.expand_str expander var_sw in
    let+ () = Action_builder.env_var var in
    []
  | Sandbox_config sandbox_config ->
    let+ () = Action_builder.dep (Dep.sandbox_config sandbox_config) in
    []

let prepare_expander expander =
  Expander.set_expanding_what expander Deps_like_field

let unnamed ~expander l =
  let expander = prepare_expander expander in
  List.fold_left l ~init:(Action_builder.return ()) ~f:(fun acc x ->
      let+ () = acc
      and+ _x = dep expander x in
      ())

let named ~expander l =
  let builders, bindings =
    let expander = prepare_expander expander in
    List.fold_left l ~init:([], Pform.Map.empty)
      ~f:(fun (builders, bindings) x ->
        match x with
        | Bindings.Unnamed x -> (dep expander x :: builders, bindings)
        | Named (name, x) ->
          let x =
            Action_builder.memoize ("dep " ^ name)
              (let+ l = Action_builder.all (List.map x ~f:(dep expander)) in
               List.concat l)
          in
          let bindings =
            Pform.Map.set bindings (Var (User_var name))
              (let+ paths = x in
               Dune_util.Value.L.paths paths)
          in
          (x :: builders, bindings))
  in
  let builder =
    let+ l = Action_builder.all (List.rev builders) in
    Dune_util.Value.L.paths (List.concat l)
  in
  let builder = Action_builder.memoize "deps" builder in
  let bindings = Pform.Map.set bindings (Var Deps) builder in
  let expander = Expander.add_bindings_full expander ~bindings in
  let builder = Action_builder.ignore builder in
  (builder, expander)
