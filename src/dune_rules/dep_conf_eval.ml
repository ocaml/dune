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

(* Package metadata may advertise libraries whose dependencies are unavailable
   in the current context. Such libraries are not usable roots. *)
let available_library_closure lib =
  let open Memo.O in
  let compile_closure = Lib.closure [ lib ] ~linking:false ~for_:Compilation_mode.Ocaml in
  let* available = Resolve.Memo.is_ok compile_closure in
  if not available
  then Memo.return []
  else
    let* compile_closure = Resolve.Memo.read_memo compile_closure
    and* link_closure =
      Lib.partial_link_closure [ lib ] ~for_:Compilation_mode.Ocaml
      |> Resolve.Memo.read_memo
    in
    Memo.return (List.rev_append compile_closure link_closure)
;;

type library_support =
  { workspace_libraries : Install_layout.Library.Set.t
  ; external_files : Path.Set.t
  }

let empty_library_support =
  { workspace_libraries = Install_layout.Library.Set.empty
  ; external_files = Path.Set.empty
  }
;;

let external_library_files lib =
  let open Memo.O in
  let info = Lib.info lib in
  let modules =
    match Lib_info.modules info ~for_:Compilation_mode.Ocaml with
    | Local | External None -> None
    | External (Some modules) -> Some modules
  in
  let* module_files =
    match modules with
    | Some modules ->
      let obj_dir = Lib_info.obj_dir info in
      let { Lib_mode.Map.ocaml = { Mode.Dict.byte; native }; melange = _ } =
        Lib_info.modes info
      in
      Modules.With_vlib.obj_map modules
      |> Module_name.Unique.Map.values
      |> List.rev_concat_map ~f:(fun module_ ->
        let module_ = Modules.Sourced_module.to_module module_ in
        let cm_file kind = Obj_dir.Module.cm_file obj_dir module_ ~kind in
        let cm_file_if enabled kind = if enabled then cm_file kind else None in
        let virtual_files =
          match Lib_info.kind info with
          | Virtual ->
            [ cm_file_if byte (Lib_mode.Cm_kind.Ocaml Cmo)
            ; cm_file_if native (Lib_mode.Cm_kind.Ocaml Cmx)
            ]
          | Dune_file _ | Parameter -> []
        in
        List.filter_opt (cm_file (Lib_mode.Cm_kind.Ocaml Cmi) :: virtual_files))
      |> Memo.return
    | None ->
      (* META files do not list their modules. Scan only the library's immediate
         directory so that nested findlib subpackages remain excluded. *)
      let dir = Lib_info.src_dir info in
      Fs.dir_contents dir
      >>| (function
       | Error error -> Unix_error.Detailed.raise error
       | Ok files ->
         List.filter_map files ~f:(fun (file, kind) ->
           match Filename.extension file |> Filename.Extension.Or_empty.extension with
           | Some extension
             when (File_kind.equal kind S_REG || File_kind.equal kind S_LNK)
                  && List.mem
                       [ Filename.Extension.cmi
                       ; Filename.Extension.cmo
                       ; Filename.Extension.cmx
                       ]
                       extension
                       ~equal:Filename.Extension.equal ->
             Path.relative_fname dir file |> Option.some
           | None | Some _ -> None))
  in
  let foreign_objects =
    (* [dune-package] records foreign objects for every library, but only
       virtual-library foreign objects are installed. *)
    match Lib_info.kind info with
    | Virtual ->
      (match Lib_info.foreign_objects info with
       | Local -> []
       | External files -> files)
    | Parameter | Dune_file _ -> []
  in
  let public_headers =
    match Lib_info.public_headers info with
    | Local _ -> []
    | External files -> files
  in
  let metadata_file =
    let loc = Lib_info.loc info in
    if Loc.is_none loc then [] else [ Path.of_string (Loc.start loc).pos_fname ]
  in
  let { Mode.Dict.byte; native } = Lib_info.archives info in
  List.rev_concat
    [ metadata_file
    ; module_files
    ; byte
    ; native
    ; Lib_info.eval_native_archives_exn info ~modules
    ; Mode.Map.Multi.to_flat_list (Lib_info.foreign_archives info)
    ; foreign_objects
    ; public_headers
    ; (Lib_info.plugins info).byte
    ; (Lib_info.plugins info).native
    ; Lib_info.foreign_dll_files info
    ; Lib_info.jsoo_runtime info
    ; Lib_info.wasmoo_runtime info
    ]
  |> Path.Set.of_list
  |> Memo.return
;;

let installed_library_root_names (package : Dune_package.t) =
  Lib_name.Map.values package.entries
  |> List.filter_map ~f:(function
    | Dune_package.Entry.Library lib -> Some (Lib_info.name (Dune_package.Lib.info lib))
    | Deprecated_library_name { new_public_name; _ } -> Some new_public_name
    | Hidden_library _ -> None)
;;

let library_support_closure
      context
      ~packages
      ~workspace_packages
      ~installed_library_names
      ~build_packages
  =
  let open Memo.O in
  let* public_libs = Scope.DB.public_libs context in
  let* local_roots =
    Package.Name.Set.to_list workspace_packages
    |> Memo.parallel_map ~f:(fun package ->
      let+ { Scope.DB.Lib_entry.Set.libraries; deprecated_library_names = _ } =
        Scope.DB.lib_entries_of_package context package
      in
      List.rev_map libraries ~f:Lib.Local.to_lib)
    >>| List.concat
  and* installed_roots =
    Lib_name.Set.to_list installed_library_names
    |> Memo.parallel_map ~f:(Lib.DB.find public_libs)
    >>| List.filter_opt
  and* build_libraries =
    if Package.Name.Set.is_empty build_packages
    then Memo.return Lib.Set.empty
    else Lib.DB.all ~recursive:true public_libs
  in
  let roots =
    Lib.Set.fold
      build_libraries
      ~init:(List.rev_append local_roots installed_roots)
      ~f:(fun lib roots ->
        match Lib_info.package (Lib.info lib) with
        | Some package when Package.Name.Set.mem build_packages package -> lib :: roots
        | None | Some _ -> roots)
  in
  let* closure =
    Memo.parallel_map roots ~f:available_library_closure
    >>| List.concat
    >>| Lib.Set.of_list
  in
  Memo.List.fold_left
    (Lib.Set.to_list closure)
    ~init:empty_library_support
    ~f:(fun support lib ->
      match Lib_info.package (Lib.info lib) with
      | Some package when Package.Name.Set.mem packages package -> Memo.return support
      | package ->
        if Lib.is_local lib
        then
          Memo.return
            (match package with
             | Some package ->
               { support with
                 workspace_libraries =
                   Install_layout.Library.Set.add
                     support.workspace_libraries
                     (Install_layout.Library.make ~package ~name:(Lib.name lib))
               }
             | None -> support)
        else
          let+ files = external_library_files lib in
          { support with external_files = Path.Set.union support.external_files files })
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
  let package_names =
    List.filter_map classified ~f:(fun (_, package, found) ->
      Option.map found ~f:(fun _ -> package))
    |> Package.Name.Set.of_list
  in
  let local_package_names =
    List.filter_map classified ~f:(fun (_, _, found) ->
      match found with
      | Some (Package_db.Local pkg) -> Some (Package.name pkg)
      | _ -> None)
    |> Package.Name.Set.of_list
  in
  let installed_library_names =
    List.concat_map classified ~f:(fun (_, _, found) ->
      match found with
      | Some (Package_db.Installed package) -> installed_library_root_names package
      | Some (Build _ | Local _) | None -> [])
    |> Lib_name.Set.of_list
  in
  let build_package_names =
    List.filter_map classified ~f:(fun (_, package, found) ->
      match found with
      | Some (Package_db.Build _) -> Some package
      | Some (Installed _ | Local _) | None -> None)
    |> Package.Name.Set.of_list
  in
  let* { workspace_libraries; external_files } =
    if Package.Name.Set.is_empty package_names
    then Action_builder.return empty_library_support
    else
      Action_builder.of_memo
        (library_support_closure
           context.name
           ~packages:package_names
           ~workspace_packages:local_package_names
           ~installed_library_names
           ~build_packages:build_package_names)
  in
  let* env =
    if
      Package.Name.Set.is_empty local_package_names
      && Install_layout.Library.Set.is_empty workspace_libraries
    then Action_builder.return Env.empty
    else Install_layout.env context.name local_package_names workspace_libraries
  in
  let* () = Action_builder.paths (Path.Set.to_list external_files) in
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
