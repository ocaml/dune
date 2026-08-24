open Import
open Action_builder.O

let make_sandboxing_config config =
  let loc = Dep_conf.Sandbox_config.loc config in
  Dep_conf.Sandbox_config.fold config ~init:[] ~f:(fun kind acc ->
    let partial =
      match kind with
      | `None -> Sandbox_config.Partial.no_sandboxing
      | `Always -> Sandbox_config.Partial.needs_sandboxing
      | `Preserve_file_kind -> Sandbox_config.Partial.disallow Sandbox_mode.symlink
      | `Patch_back_source_tree -> Sandbox_config.Partial.patch_back_source_tree
    in
    partial :: acc)
  |> Dune_engine.Sandbox_config.Partial.merge ~loc
;;

let make_alias expander s =
  let loc = String_with_vars.loc s in
  Expander.expand_path expander s >>| Alias.of_user_written_path ~loc
;;

let expand_package_name expander swv =
  let loc = String_with_vars.loc swv in
  Expander.expand_str expander swv >>| fun s -> Package.Name.parse_string_exn (loc, s)
;;

type dep_evaluation_result =
  | Simple of Path.t list Memo.t
  | Other of Path.t list Action_builder.t
  | Include_result of (Path.t list * Env.t) Action_builder.t

let to_action_builder = function
  | Simple paths ->
    let* paths = Action_builder.of_memo paths in
    let+ () = Action_builder.all_unit (List.map ~f:Action_builder.path paths) in
    paths
  | Other x -> x
  | Include_result pair -> Action_builder.map pair ~f:fst
;;

let include_action_env = function
  | Simple _ | Other _ -> None
  | Include_result pair -> Some (Action_builder.map pair ~f:snd)
;;

let dep_on_alias_rec alias ~loc =
  let src_dir = Path.Build.drop_build_context_exn (Alias.dir alias) in
  Action_builder.of_memo (Source_tree.find_dir src_dir)
  >>= function
  | None ->
    Action_builder.fail
      { fail =
          (fun () ->
            User_error.raise
              ~loc
              [ Pp.textf
                  "Don't know about directory %s!"
                  (Path.Source.to_string_maybe_quoted src_dir)
              ])
      }
  | Some _ ->
    let name = Dune_engine.Alias.name alias in
    Alias_rec.dep_on_alias_rec name (Alias.dir alias)
    >>| (function
     | Defined -> ()
     | Not_defined ->
       if not (Alias0.is_standard name)
       then
         User_error.raise
           ~loc
           [ Pp.text "This alias is empty."
           ; Pp.textf
               "Alias %S is not defined in %s or any of its descendants."
               (Alias.Name.to_string name)
               (Path.Source.to_string_maybe_quoted src_dir)
           ])
;;

let expand_include =
  (* CR-someday rgrinberg: move this into [Dune_project]? *)
  let dep_parser project =
    Dune_lang.Syntax.set
      Stanza.syntax
      (Active (Dune_project.dune_version project))
      (String_with_vars.set_decoding_env
         (* CR-someday rgrinberg: this environment looks fishy *)
         (Pform.Env.initial ~stanza:Stanza.latest_version ~extensions:[])
         Dep_conf.decode_bindings)
  in
  fun ~dir ~project s ->
    Path.Build.relative dir s
    |> Path.build
    |> Action_builder.read_sexp
    >>| function
    | Dune_lang.Ast.List (_loc, asts) ->
      List.concat_map
        asts
        ~f:(Dune_lang.Decoder.parse (dep_parser project) Univ_map.empty)
    | ast ->
      let loc = Dune_lang.Ast.loc ast in
      User_error.raise
        ~loc
        [ Pp.text "Dependency specification in `(include <filename>)` must be a list" ]
;;

let prepare_expander expander = Expander.set_expanding_what expander Deps_like_field

let bin_dep_name (dep : Dep_conf.t) =
  match dep with
  | File s ->
    (match String_with_vars.pform_only s with
     | Some (Macro ({ macro = Bin; _ } as m)) ->
       Some (Pform.Macro_invocation.Args.whole m)
     | _ -> None)
  | _ -> None
;;

(* Returns a partial env containing only a PATH-prepend hint for the
   .binaries dir. [Super_context.extend_action] splices it onto the
   action's real PATH; it is not usable as a standalone env. *)
let make_bin_env expander bin_names =
  match bin_names with
  | [] -> Action_builder.return Env.empty
  | _ ->
    let dir = Expander.dir expander in
    let* context =
      Action_builder.of_memo
        (let open Memo.O in
         Expander.host_context expander >>| Context.name)
    in
    let* layout = Action_builder.of_memo (Bin_layout.create context ~dir bin_names) in
    (match layout with
     | None -> Action_builder.return Env.empty
     | Some (layout_dir, files) ->
       let+ () = Action_builder.paths files in
       (* The layout dir on PATH is an absolute build-tree path; dune's
          sandbox does not relocate PATH. See [bin-pform/sandbox.t]. *)
       Install.Roots.cons_path Env.empty ~var:Env_path.var layout_dir)
;;

let package_dep_swvs (dep : Dep_conf.t) =
  match dep with
  | Package p -> [ p, String_with_vars.loc p ]
  | _ -> []
;;

let add_sandbox_config acc (dep : Dep_conf.t) =
  match dep with
  | Sandbox_config cfg -> Sandbox_config.inter acc (make_sandboxing_config cfg)
  | _ -> acc
;;

let rec dir_contents ~loc d =
  let open Memo.O in
  Fs_memo.dir_contents d
  >>= function
  | Error e -> Unix_error.Detailed.raise e
  | Ok contents ->
    Fs_memo.Dir_contents.to_list contents
    |> Memo.parallel_map ~f:(fun (entry, kind) ->
      let path = Path.Outside_build_dir.relative_fname d entry in
      match kind with
      | Unix.S_REG -> Memo.return [ path ]
      | S_DIR -> dir_contents ~loc path
      | _ ->
        User_error.raise
          ~loc
          [ Pp.text "Encountered a special file while expanding dependency." ])
    >>| List.concat
;;

let package loc pkg_name (context : Build_context.t) ~dune_version =
  Action_builder.of_memo
    (let open Memo.O in
     let* package_db = Package_db.create context.name in
     Package_db.find_package package_db pkg_name)
  >>= function
  | Some (Build build) -> build
  | Some (Local _) ->
    (* The named/unnamed paths skip [Package _] before reaching here so that
       [combined_package_deps_builder] handles the whole package set at once.
       This arm only fires from [unnamed_get_paths] (e.g.
       [(public_headers (package foo))]) where a no-op is fine. *)
    Action_builder.return ()
  | Some (Installed pkg) ->
    if dune_version < (2, 9)
    then
      Action_builder.fail
        { fail =
            (fun () ->
              User_error.raise
                ~loc
                [ Pp.textf
                    "Dependency on an installed package requires at least (lang dune 2.9)"
                ])
        }
    else
      (let open Memo.O in
       Memo.parallel_map pkg.files ~f:(fun (s, l) ->
         let dir = Section.Map.find_exn pkg.sections s in
         Memo.parallel_map l ~f:(fun { kind; dst } ->
           let path = Path.append_local dir (Install.Entry.Dst.local dst) in
           match kind with
           | File -> Memo.return [ path ]
           | Directory ->
             Path.as_outside_build_dir_exn path
             |> dir_contents ~loc
             >>| List.rev_map ~f:Path.outside_build_dir)
         >>| List.concat)
       >>| List.concat)
      |> Action_builder.of_memo
      >>= Action_builder.paths
  | None ->
    Action_builder.fail
      { fail =
          (fun () ->
            User_error.raise
              ~loc
              [ Pp.textf "Package %s does not exist" (Package.Name.to_string pkg_name) ])
      }
;;

let library_closure lib =
  let open Memo.O in
  Memo.parallel_map [ Compilation_mode.Ocaml; Melange ] ~f:(fun for_ ->
    let* compile_closure =
      Lib.closure [ lib ] ~linking:false ~for_ |> Resolve.Memo.read_memo
    and* link_closure =
      Lib.partial_link_closure [ lib ] ~for_ |> Resolve.Memo.read_memo
    in
    Memo.return (List.rev_append compile_closure link_closure))
  >>| List.concat
  >>| Lib.Set.of_list
  >>| Lib.Set.to_list
;;

let workspace_redirects context =
  let open Memo.O in
  let* workspace_packages = Dune_load.packages () in
  Package.Name.Map.keys workspace_packages
  |> Memo.parallel_map ~f:(fun package ->
    let+ { Scope.DB.Lib_entry.Set.deprecated_library_names; _ } =
      Scope.DB.lib_entries_of_package context package
    in
    List.map
      deprecated_library_names
      ~f:(fun { Library_redirect.old_name; new_public_name; _ } ->
        Public_lib.name (fst old_name), (package, snd new_public_name)))
  >>| List.concat
  >>| Lib_name.Map.of_list_reduce ~f:(fun redirect _ -> redirect)
;;

let library_support_closure context packages =
  let open Memo.O in
  let* public_libs = Scope.DB.public_libs context
  and* redirects_by_name = workspace_redirects context in
  let collect_redirects name =
    let rec loop name seen redirects =
      if Lib_name.Set.mem seen name
      then redirects
      else (
        let seen = Lib_name.Set.add seen name in
        match Lib_name.Map.find redirects_by_name name with
        | None -> redirects
        | Some (package, target) ->
          let redirects =
            if Package.Name.Set.mem packages package
            then redirects
            else
              Install_layout.Redirect.Set.add
                redirects
                (Install_layout.Redirect.make ~package ~name)
          in
          loop target seen redirects)
    in
    loop name Lib_name.Set.empty Install_layout.Redirect.Set.empty
  in
  let* roots, redirects =
    Package.Name.Set.to_list packages
    |> Memo.parallel_map ~f:(fun package ->
      let* { Scope.DB.Lib_entry.Set.libraries; deprecated_library_names } =
        Scope.DB.lib_entries_of_package context package
      in
      let+ redirect_targets =
        Memo.parallel_map deprecated_library_names ~f:(fun { new_public_name; _ } ->
          let+ target =
            Lib.DB.resolve public_libs new_public_name |> Resolve.Memo.read_memo
          in
          target, collect_redirects (snd new_public_name))
      in
      ( List.rev_append
          (List.rev_map libraries ~f:Lib.Local.to_lib)
          (List.map redirect_targets ~f:fst)
      , List.fold_left
          redirect_targets
          ~init:Install_layout.Redirect.Set.empty
          ~f:(fun redirects (_, selected) ->
            Install_layout.Redirect.Set.union redirects selected) ))
    >>| List.split
    >>| fun (roots, redirects) ->
    ( List.concat roots
    , List.fold_left
        redirects
        ~init:Install_layout.Redirect.Set.empty
        ~f:Install_layout.Redirect.Set.union )
  in
  let extra_dependencies libraries =
    Memo.parallel_map libraries ~f:(fun lib ->
      if Lib.is_local lib
      then
        Memo.parallel_map [ Compilation_mode.Ocaml; Melange ] ~f:(fun for_ ->
          Lib.ppx_runtime_deps lib ~for_ |> Resolve.Memo.read_memo)
        >>| List.concat
      else Memo.return [])
    >>| List.concat
  in
  let rec loop todo expanded inspected libraries =
    match todo with
    | [] -> Memo.return libraries
    | lib :: todo ->
      if Lib.Set.mem expanded lib
      then loop todo expanded inspected libraries
      else
        let* closure = library_closure lib in
        let expanded = Lib.Set.add expanded lib in
        let uninspected =
          List.filter closure ~f:(fun lib -> not (Lib.Set.mem inspected lib))
        in
        let inspected = List.fold_left uninspected ~init:inspected ~f:Lib.Set.add in
        let libraries = List.fold_left closure ~init:libraries ~f:Lib.Set.add in
        let* extra_dependencies = extra_dependencies uninspected in
        loop (List.rev_append extra_dependencies todo) expanded inspected libraries
  in
  let+ closure = loop roots Lib.Set.empty Lib.Set.empty Lib.Set.empty in
  let libraries =
    Lib.Set.fold closure ~init:Install_layout.Library.Set.empty ~f:(fun lib libraries ->
      match Lib.is_local lib, Lib_info.package (Lib.info lib) with
      | true, Some package when not (Package.Name.Set.mem packages package) ->
        Install_layout.Library.Set.add
          libraries
          (Install_layout.Library.make ~package ~name:(Lib.name lib))
      | _ -> libraries)
  in
  libraries, redirects
;;

let rec dep expander : Dep_conf.t -> _ = function
  | Include s ->
    (* TODO this is wrong. we shouldn't allow bindings here if we are in an
       unnamed expansion *)
    let dir = Expander.dir expander in
    let pair =
      let* deps =
        let* project = Action_builder.of_memo @@ Dune_load.find_project ~dir in
        expand_include ~dir ~project s
      in
      let builder, _bindings, action_env = named_paths_builder ~expander deps in
      let+ paths = builder
      and+ env = action_env in
      paths, env
    in
    Include_result (Action_builder.memoize "include-eval" pair)
  | File s ->
    let expanded = Expander.With_deps_if_necessary.expand_path expander s in
    (match String_with_vars.pform_only s with
     | Some (Macro { macro = Melange_emit; _ }) ->
       (match expanded with
        | Without paths -> Simple paths
        | With paths -> Other paths)
     | _ ->
       (match expanded with
        | Without paths ->
          (* This special case is to support this pattern:

             {v
... (deps (:x foo)) (action (... (diff? %{x} %{x}.corrected))) ...
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
             let+ () = Action_builder.all_unit (List.map ~f:Action_builder.path paths) in
             paths)))
  | Alias s ->
    Other
      (let* a = make_alias expander s in
       let+ () = Alias_builder.alias a in
       [])
  | Alias_rec s ->
    Other
      (let* a = make_alias expander s in
       let+ () = dep_on_alias_rec ~loc:(String_with_vars.loc s) a in
       [])
  | Glob_files glob_files ->
    Other
      (Glob_files_expand.action_builder
         glob_files
         ~f:(Expander.expand ~mode:Single expander)
         ~base_dir:(Expander.dir expander)
       >>| Glob_files_expand.Expanded.matches
       >>| List.map ~f:(fun path ->
         if Filename.is_relative path
         then Path.Build.relative (Expander.dir expander) path |> Path.build
         else Path.of_string path))
  | Source_tree s ->
    Other
      (let* path = Expander.expand_path expander s in
       let deps = Source_deps.files path in
       Action_builder.dyn_memo_deps deps |> Action_builder.map ~f:Path.Set.to_list)
  | Package p ->
    Other
      (let+ () =
         let* pkg_name = expand_package_name expander p in
         let context = Build_context.create ~name:(Expander.context expander) in
         let loc = String_with_vars.loc p in
         let dune_version = Expander.project expander |> Dune_project.dune_version in
         package loc pkg_name context ~dune_version
       in
       [])
  | Universe ->
    Other
      (let+ () = Action_builder.dep Dep.universe in
       [])
  | Env_var var_sw ->
    Other
      (let* var = Expander.expand_str expander var_sw in
       let+ () = Action_builder.env_var (Env.Var.of_string var) in
       [])
  | Sandbox_config _ -> Other (Action_builder.return [])

and combined_package_deps_builder expander pkgs =
  let open Action_builder.O in
  (* Resolve packages and name the layout dir in the host context, same as
     [make_bin_env] above. *)
  let* host_name =
    Action_builder.of_memo
      (let open Memo.O in
       Expander.host_context expander >>| Context.name)
  in
  let context = Build_context.create ~name:host_name in
  let* package_db = Action_builder.of_memo (Package_db.create context.name) in
  let* classified =
    Action_builder.List.map pkgs ~f:(fun (swv, loc) ->
      let* pkg = expand_package_name expander swv in
      let+ found = Action_builder.of_memo (Package_db.find_package package_db pkg) in
      loc, pkg, found)
  in
  let local_package_names =
    List.filter_map classified ~f:(fun (_, _, found) ->
      match found with
      | Some (Package_db.Local pkg) -> Some (Package.name pkg)
      | _ -> None)
    |> Package.Name.Set.of_list
  in
  let* env =
    if Package.Name.Set.is_empty local_package_names
    then Action_builder.return Env.empty
    else
      let* support_libraries, support_redirects =
        Action_builder.of_memo (library_support_closure context.name local_package_names)
      in
      Install_layout.env
        context.name
        local_package_names
        support_libraries
        support_redirects
  in
  let dune_version = Expander.project expander |> Dune_project.dune_version in
  let+ () =
    Action_builder.List.iter classified ~f:(fun (loc, pkg_name, found) ->
      match found with
      | Some (Local _) -> Action_builder.return ()
      | Some (Build build) -> build
      | Some (Installed _) | None -> package loc pkg_name context ~dune_version)
  in
  env

and named_paths_builder ~expander l =
  let builders, bindings, combined_packages_builder, bin_names, include_envs =
    let expander = prepare_expander expander in
    let package_swvs =
      List.concat_map l ~f:(function
        | Bindings.Unnamed dep -> package_dep_swvs dep
        | Bindings.Named (_, deps) -> List.concat_map deps ~f:package_dep_swvs)
    in
    let bin_names =
      List.concat_map l ~f:(function
        | Bindings.Unnamed dep -> Option.to_list (bin_dep_name dep)
        | Bindings.Named (_, deps) -> List.filter_map deps ~f:bin_dep_name)
    in
    let combined_packages_builder =
      match package_swvs with
      | [] -> None
      | pkgs -> Some (combined_package_deps_builder expander pkgs)
    in
    let builders, bindings, include_envs =
      List.fold_left
        l
        ~init:([], Pform.Map.empty, [])
        ~f:(fun (builders, bindings, envs) x ->
          match x with
          | Bindings.Unnamed (Dep_conf.Package _)
            when Option.is_some combined_packages_builder -> builders, bindings, envs
          | Bindings.Unnamed x ->
            let r = dep expander x in
            let envs =
              match include_action_env r with
              | Some e -> e :: envs
              | None -> envs
            in
            to_action_builder r :: builders, bindings, envs
          | Named (name, x) ->
            let x =
              List.map x ~f:(function
                | Dep_conf.Package p ->
                  User_error.raise
                    ~loc:(String_with_vars.loc p)
                    ~hints:
                      [ Pp.text "Place the (package ...) entry in the deps list directly."
                      ]
                    [ Pp.textf
                        "(package ...) is not supported inside a named dependency \
                         binding (:%s)."
                        name
                    ]
                | d -> dep expander d)
            in
            let envs =
              List.fold_left x ~init:envs ~f:(fun envs r ->
                match include_action_env r with
                | Some e -> e :: envs
                | None -> envs)
            in
            (match
               Option.List.all
                 (List.map x ~f:(function
                    | Simple x -> Some x
                    | Other _ | Include_result _ -> None))
             with
             | Some x ->
               let open Memo.O in
               let x =
                 Memo.lazy_ ~name:"named-dependency-bindings" (fun () ->
                   Memo.all_concurrently x >>| List.concat)
               in
               let bindings =
                 Pform.Map.set
                   bindings
                   (Var (User_var name))
                   (Expander.Deps.Without (Memo.Lazy.force x >>| Value.L.paths))
               in
               let x =
                 let open Action_builder.O in
                 let* x = Action_builder.of_memo (Memo.Lazy.force x) in
                 let+ () = Action_builder.paths x in
                 x
               in
               x :: builders, bindings, envs
             | None ->
               let x =
                 Action_builder.memoize
                   ~cutoff:(List.equal Path.equal)
                   ("dep " ^ name)
                   (Action_builder.List.concat_map x ~f:to_action_builder)
               in
               let bindings =
                 Pform.Map.set
                   bindings
                   (Var (User_var name))
                   (Expander.Deps.With (x >>| Value.L.paths))
               in
               x :: builders, bindings, envs))
    in
    builders, bindings, combined_packages_builder, bin_names, include_envs
  in
  let builders, package_env =
    match combined_packages_builder with
    | None -> builders, Action_builder.return Env.empty
    | Some b ->
      let open Action_builder.O in
      let b = Action_builder.memoize "combined-package-deps" b in
      (b >>| fun _ -> []) :: builders, b
  in
  let bin_env = make_bin_env expander bin_names in
  let action_env =
    let outer =
      let open Action_builder.O in
      let+ package_env = package_env
      and+ bin_env = bin_env in
      Env_path.extend_env_concat_path package_env bin_env
    in
    List.fold_left include_envs ~init:outer ~f:(fun acc env ->
      let open Action_builder.O in
      let+ acc = acc
      and+ env = env in
      Install.Roots.extend_env_concat_path_vars acc env)
  in
  let builder = List.rev builders |> Action_builder.all >>| List.concat in
  builder, bindings, action_env
;;

let named sandbox ~expander l =
  let builder, bindings, action_env = named_paths_builder ~expander l in
  let builder =
    Action_builder.memoize
      ~cutoff:(List.equal Value.equal)
      "deps"
      (builder >>| Value.L.paths)
  in
  let bindings = Pform.Map.set bindings (Var Deps) (Expander.Deps.With builder) in
  let expander = Expander.add_bindings_full expander ~bindings in
  let sandbox =
    let open Action_builder.O in
    let rec sandbox_dep acc = function
      | Dep_conf.Include s ->
        let* deps =
          let dir = Expander.dir expander in
          let* project = Action_builder.of_memo (Dune_load.find_project ~dir) in
          expand_include ~dir ~project s
        in
        sandbox_bindings acc deps
      | dep -> Action_builder.return (add_sandbox_config acc dep)
    and sandbox_bindings acc deps =
      Bindings.fold deps ~init:(Action_builder.return acc) ~f:(fun one acc ->
        let* acc = acc in
        match one with
        | Unnamed dep -> sandbox_dep acc dep
        | Named (_, deps) -> Action_builder.List.fold_left deps ~init:acc ~f:sandbox_dep)
    in
    sandbox_bindings sandbox l
    |> Action_builder.memoize ~cutoff:Sandbox_config.equal "deps sandbox"
  in
  let action_env =
    Action_builder.memoize
      "deps action_env"
      (let+ _paths = builder
       and+ env = action_env in
       env)
  in
  action_env, expander, sandbox
;;

let unnamed sandbox ~expander l =
  let expander = prepare_expander expander in
  let package_swvs = List.concat_map l ~f:package_dep_swvs in
  let package_env =
    match package_swvs with
    | [] -> Action_builder.return Env.empty
    | pkgs -> combined_package_deps_builder expander pkgs
  in
  let has_combined = not (List.is_empty package_swvs) in
  let bin_names = List.filter_map l ~f:bin_dep_name in
  let include_envs =
    List.fold_left l ~init:[] ~f:(fun envs x ->
      match include_action_env (dep expander x) with
      | Some e -> e :: envs
      | None -> envs)
  in
  let bin_env =
    let outer = make_bin_env expander bin_names in
    List.fold_left include_envs ~init:outer ~f:(fun acc env ->
      let open Action_builder.O in
      let+ acc = acc
      and+ env = env in
      Install.Roots.extend_env_concat_path_vars acc env)
  in
  let action_env =
    Action_builder.memoize
      "deps action_env"
      (let+ () =
         List.fold_left l ~init:(Action_builder.return ()) ~f:(fun acc x ->
           match x with
           | Dep_conf.Package _ when has_combined -> acc
           | _ ->
             let+ () = acc
             and+ _x = to_action_builder (dep expander x) in
             ())
       and+ package_env = package_env
       and+ bin_env = bin_env in
       Env_path.extend_env_concat_path package_env bin_env)
  in
  action_env, List.fold_left l ~init:sandbox ~f:add_sandbox_config
;;

let unnamed_get_paths ~expander l =
  let expander = prepare_expander expander in
  ( (let+ paths =
       List.fold_left l ~init:(Action_builder.return []) ~f:(fun acc x ->
         let+ acc = acc
         and+ paths = to_action_builder (dep expander x) in
         paths :: acc)
     in
     Path.Set.of_list (List.concat paths))
  , List.fold_left l ~init:None ~f:(fun acc (config : Dep_conf.t) ->
      match acc, config with
      | None, Sandbox_config _ ->
        Some
          (add_sandbox_config
             (Option.value ~default:Sandbox_config.no_special_requirements acc)
             config)
      | _, _ -> acc) )
;;
