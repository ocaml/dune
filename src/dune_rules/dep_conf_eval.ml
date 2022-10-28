open Import
open Action_builder.O
open Dep_conf

let make_alias expander s =
  let loc = String_with_vars.loc s in
  Expander.expand_path expander s >>| Alias.of_user_written_path ~loc

let package_install ~(context : Build_context.t) ~(pkg : Package.t) =
  let dir =
    let dir = Package.dir pkg in
    Path.Build.append_source context.build_dir dir
  in
  let name = Package.name pkg in
  sprintf ".%s-files" (Package.Name.to_string name)
  |> Alias.Name.of_string |> Alias.make ~dir

module Source_tree_map_reduce =
  Source_tree.Dir.Make_map_reduce (Action_builder) (Monoid.Union (Path.Set))

let collect_source_files_recursively dir ~f =
  let prefix_with, dir = Path.extract_build_context_dir_exn dir in
  Action_builder.of_memo (Source_tree.find_dir dir) >>= function
  | None -> Action_builder.return Path.Set.empty
  | Some dir ->
    Source_tree_map_reduce.map_reduce dir ~traverse:Sub_dirs.Status.Set.all
      ~f:(fun dir ->
        f (Path.append_source prefix_with (Source_tree.Dir.path dir)))

type dep_evaluation_result =
  | Simple of Path.t list Memo.t
  | Other of Path.t list Action_builder.t

let to_action_builder = function
  | Simple paths ->
    let* paths = Action_builder.of_memo paths in
    let+ () = Action_builder.all_unit (List.map ~f:Action_builder.path paths) in
    paths
  | Other x -> x

let dep_on_alias_rec alias ~loc =
  let ctx_name, src_dir =
    Path.Build.extract_build_context_exn (Alias.dir alias)
  in
  Action_builder.of_memo (Source_tree.find_dir src_dir) >>= function
  | None ->
    Action_builder.fail
      { fail =
          (fun () ->
            User_error.raise ~loc
              [ Pp.textf "Don't know about directory %s!"
                  (Path.Source.to_string_maybe_quoted src_dir)
              ])
      }
  | Some dir ->
    let name = Dune_engine.Alias.name alias in
    let+ is_nonempty =
      Action_builder.dep_on_alias_rec name (Context_name.of_string ctx_name) dir
    in
    if (not is_nonempty) && not (Alias.is_standard name) then
      User_error.raise ~loc
        [ Pp.text "This alias is empty."
        ; Pp.textf "Alias %S is not defined in %s or any of its descendants."
            (Alias.Name.to_string name)
            (Path.Source.to_string_maybe_quoted src_dir)
        ]

let relative d s = Path.build (Path.Build.relative d s)

let expand_include ~expander s =
  let path = relative (Expander.dir expander) s in
  let+ ast = Action_builder.read_sexp path in
  match ast with
  | Dune_lang.Ast.List (_loc, asts) ->
    let dep_parser =
      Dune_lang.Syntax.set Stanza.syntax
        (Active
           (Dune_project.dune_version (Scope.project (Expander.scope expander))))
        (String_with_vars.set_decoding_env
           (Pform.Env.initial Stanza.latest_version)
           (Bindings.decode Dep_conf.decode))
    in
    List.concat_map ~f:(Dune_lang.Decoder.parse dep_parser Univ_map.empty) asts
  | ast ->
    let loc = Dune_lang.Ast.loc ast in
    User_error.raise ~loc
      [ Pp.text
          "Dependency specification in `(include <filename>)` must be a list"
      ]

let prepare_expander expander =
  Expander.set_expanding_what expander Deps_like_field

let add_sandbox_config acc (dep : Dep_conf.t) =
  match dep with
  | Sandbox_config cfg -> Sandbox_config.inter acc cfg
  | _ -> acc

let rec dep expander = function
  | Include s ->
    let deps = expand_include ~expander s in
    Other
      (let* deps = deps in
       let builder, _bindings = named_paths_builder ~expander deps in
       let+ paths = builder in
       paths)
  | File s -> (
    match Expander.With_deps_if_necessary.expand_path expander s with
    | Without paths ->
      (* This special case is to support this pattern:

         {v ... (deps (:x foo)) (action (... (diff? %{x} %{x}.corrected))) ...
         v}

         Indeed, the second argument of [diff?] must be something that can be
         evaluated at rule production time since the dependency/target inferrer
         treats this argument as "consuming a target", and targets must be known
         at rule production time. This is not compatible with computing its
         expansion in the action builder monad, which is evaluated at rule
         execution time. *)
      Simple paths
    | With paths ->
      Other
        (let* paths = paths in
         let+ () =
           Action_builder.all_unit (List.map ~f:Action_builder.path paths)
         in
         paths))
  | Alias s ->
    Other
      (let* a = make_alias expander s in
       let+ () = Action_builder.alias a in
       [])
  | Alias_rec s ->
    Other
      (let* a = make_alias expander s in
       let+ () = dep_on_alias_rec ~loc:(String_with_vars.loc s) a in
       [])
  | Glob_files { glob = s; recursive } ->
    Other
      (let loc = String_with_vars.loc s in
       let* path = Expander.expand_path expander s in
       if recursive && not (Path.is_managed path) then
         User_error.raise ~loc
           [ Pp.textf "Absolute paths in recursive globs are not supported." ];
       let files_in =
         let glob = Path.basename path |> Glob.of_string_exn loc in
         fun dir ->
           Action_builder.paths_matching ~loc (File_selector.of_glob ~dir glob)
       in
       let+ files =
         let dir = Path.parent_exn path in
         if recursive then collect_source_files_recursively dir ~f:files_in
         else files_in dir
       in
       Path.Set.to_list files)
  | Source_tree s ->
    Other
      (let* path = Expander.expand_path expander s in
       Action_builder.map ~f:Path.Set.to_list
         (Action_builder.source_tree ~dir:path))
  | Package p ->
    Other
      (let* pkg = Expander.expand_str expander p in
       let+ () =
         let pkg = Package.Name.of_string pkg in
         let context = Expander.context expander in
         let sites = Expander.sites expander in
         Action_builder.of_memo (Sites.find_package sites pkg) >>= function
         | Some (Local pkg) ->
           Action_builder.alias
             (package_install ~context:(Context.build_context context) ~pkg)
         | Some (Installed pkg) ->
           let version =
             Dune_project.dune_version @@ Scope.project
             @@ Expander.scope expander
           in
           if version < (2, 9) then
             Action_builder.fail
               { fail =
                   (fun () ->
                     let loc = String_with_vars.loc p in
                     User_error.raise ~loc
                       [ Pp.textf
                           "Dependency on an installed package requires at \
                            least (lang dune 2.9)"
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
       [])
  | Universe ->
    Other
      (let+ () = Action_builder.dep Dep.universe in
       [])
  | Env_var var_sw ->
    Other
      (let* var = Expander.expand_str expander var_sw in
       let+ () = Action_builder.env_var var in
       [])
  | Sandbox_config _ -> Other (Action_builder.return [])

and named_paths_builder ~expander l =
  let builders, bindings =
    let expander = prepare_expander expander in
    List.fold_left l ~init:([], Pform.Map.empty)
      ~f:(fun (builders, bindings) x ->
        match x with
        | Bindings.Unnamed x ->
          (to_action_builder (dep expander x) :: builders, bindings)
        | Named (name, x) -> (
          let x = List.map x ~f:(dep expander) in
          match
            Option.List.all
              (List.map x ~f:(function
                | Simple x -> Some x
                | Other _ -> None))
          with
          | Some x ->
            let open Memo.O in
            let x = Memo.lazy_ (fun () -> Memo.all x) in
            let bindings =
              Pform.Map.set bindings (Var (User_var name))
                (Expander.Deps.Without
                   (let+ paths = Memo.Lazy.force x in
                    Dune_util.Value.L.paths (List.concat paths)))
            in
            let x =
              let open Action_builder.O in
              let* x = Action_builder.of_memo (Memo.Lazy.force x) in
              let x = List.concat x in
              let+ () = Action_builder.paths x in
              x
            in
            (x :: builders, bindings)
          | None ->
            let x =
              Action_builder.memoize ~cutoff:(List.equal Path.equal)
                ("dep " ^ name)
                (Action_builder.List.concat_map x ~f:to_action_builder)
            in
            let bindings =
              Pform.Map.set bindings (Var (User_var name))
                (Expander.Deps.With
                   (let+ paths = x in
                    Dune_util.Value.L.paths paths))
            in
            (x :: builders, bindings)))
  in
  let builder =
    let+ l = Action_builder.all (List.rev builders) in
    List.concat l
  in
  (builder, bindings)

let named ~expander l =
  let builder, bindings = named_paths_builder ~expander l in
  let builder =
    let+ paths = builder in
    Dune_util.Value.L.paths paths
  in
  let builder =
    Action_builder.memoize ~cutoff:(List.equal Value.equal) "deps" builder
  in
  let bindings =
    Pform.Map.set bindings (Var Deps) (Expander.Deps.With builder)
  in
  let expander = Expander.add_bindings_full expander ~bindings in
  ( Action_builder.ignore builder
  , expander
  , Bindings.fold l ~init:Sandbox_config.no_special_requirements
      ~f:(fun one acc ->
        match one with
        | Unnamed dep -> add_sandbox_config acc dep
        | Named (_, l) -> List.fold_left l ~init:acc ~f:add_sandbox_config) )

let unnamed ?(sandbox = Sandbox_config.no_special_requirements) ~expander l =
  let expander = prepare_expander expander in
  ( List.fold_left l ~init:(Action_builder.return ()) ~f:(fun acc x ->
        let+ () = acc
        and+ _x = to_action_builder (dep expander x) in
        ())
  , List.fold_left l ~init:sandbox ~f:add_sandbox_config )
