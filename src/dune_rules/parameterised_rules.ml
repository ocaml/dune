open Import
open Memo.O

let obj_file ~obj_dir ~kind ?ext unique_name =
  let ext =
    match ext with
    | None -> Lib_mode.Cm_kind.ext kind
    | Some ext -> ext
  in
  Obj_dir.Module.obj_file_of_name obj_dir unique_name ~kind ~ext ~visibility:Public
;;

let get_cm ~kind lib =
  let open Resolve.O in
  let+ name = Lib.Parameterised.applied_name lib in
  let unique_name = Parameterised_name.to_module_name name in
  let obj_dir = Lib_info.obj_dir (Lib.info lib) in
  obj_file ~obj_dir ~kind unique_name
;;

type t =
  { module_ : Path.t
  ; args : Path.t list Resolve.t
  ; requires : Lib.t list Resolve.t
  ; target : Path.Build.t
  ; o_target : Path.Build.t option
  ; hidden_targets : Path.Build.t list
  ; hidden_deps : Dep.Set.t Action_builder.t
  }

let for_ = Compilation_mode.Ocaml

let build_instance
      ~sctx
      ~obj_dir
      ~mode
      { module_; args; requires; target; o_target = _; hidden_targets; hidden_deps }
  =
  let ctx = Super_context.context sctx in
  let* ocaml = Context.ocaml ctx in
  let include_flags =
    let open Action_builder.O in
    let+ requires = Resolve.read requires in
    Lib_flags.L.include_flags
      ~direct_libs:requires
      ~hidden_libs:[]
      (Ocaml mode)
      ocaml.lib_config
  in
  let dir =
    let cm_kind = Lib_mode.Cm_kind.Ocaml (Mode.cm_kind mode) in
    Obj_dir.cm_dir obj_dir cm_kind Public
  in
  Ocaml_toolchain.compiler ocaml mode
  |> Memo.Result.iter ~f:(fun compiler ->
    [ Command.Args.Dyn include_flags
    ; S
        (List.concat_map
           ~f:(fun dir -> [ Command.Args.A "-H"; Path (Path.build dir) ])
           (Obj_dir.all_obj_dirs ~mode:(Lib_mode.Ocaml mode) obj_dir))
    ; A "-w"
    ; A "-55"
      (* CR art-w: ignore [inlining-impossible] warning, it's unclear
         why it happens *)
    ; A "-instantiate"
    ; Dep module_
    ; Dyn
        (let open Action_builder.O in
         let+ args = Resolve.read args in
         Command.Args.Deps args)
    ; Dyn
        (let open Action_builder.O in
         let+ hidden_deps = hidden_deps in
         Command.Args.Hidden_deps hidden_deps)
    ; A "-o"
    ; Target target
    ; Hidden_targets hidden_targets
    ]
    |> Command.run
         ~sandbox:Sandbox_config.needs_sandboxing
         (Ok compiler)
         ~dir:(Path.build dir)
    |> Super_context.add_rule ~dir sctx)
;;

let build_archive ~sctx ~mode ~obj_dir ~lib ~top_sorted_modules ~modules =
  let lib_info = Lib_info.as_local_exn (Lib.info lib) in
  let target =
    match Mode.Dict.get (Lib_info.archives lib_info) mode with
    | [ target ] -> target
    | [] | _ :: _ :: _ ->
      Code_error.raise
        "expected single target"
        [ "info", Lib_info.to_dyn Path.Build.to_dyn lib_info ]
  in
  let hidden_targets =
    match mode, Lib_info.native_archives lib_info with
    | Native, Files lst -> lst
    | Byte, _ -> []
    | Native, Needs_module_info _ ->
      Code_error.raise
        "expected native archive files"
        [ "info", Lib_info.to_dyn Path.Build.to_dyn lib_info ]
  in
  let dir =
    let cm_kind = Lib_mode.Cm_kind.Ocaml (mode |> Mode.cm_kind) in
    Obj_dir.cm_dir obj_dir cm_kind Public
  in
  let* ocaml = Context.ocaml (Super_context.context sctx) in
  Ocaml_toolchain.compiler ocaml mode
  |> Memo.Result.iter ~f:(fun compiler ->
    [ Command.Args.S
        (List.concat_map
           ~f:(fun dir -> [ Command.Args.A "-H"; Path (Path.build dir) ])
           (Obj_dir.all_obj_dirs ~mode:(Lib_mode.Ocaml mode) obj_dir))
    ; A "-a"
    ; Dyn
        (let open Action_builder.O in
         let+ top_sorted_modules = top_sorted_modules in
         let deps =
           List.filter_map top_sorted_modules ~f:(fun m ->
             let name = Module.name m in
             match Module_name.Map.find modules name with
             | None -> None
             | Some inst -> Some (Path.build inst.target))
         in
         Command.Args.Deps deps)
    ; Dyn
        (let open Action_builder.O in
         let+ top_sorted_modules = top_sorted_modules in
         let deps =
           Dep.Set.of_list
           @@ List.filter_map top_sorted_modules ~f:(fun m ->
             let name = Module.name m in
             match Module_name.Map.find modules name with
             | Some { o_target = Some o_target; _ } ->
               Some (Dep.file (Path.build o_target))
             | None | Some { o_target = None; _ } -> None)
         in
         Command.Args.Hidden_deps deps)
    ; A "-o"
    ; Target target
    ; Hidden_targets hidden_targets
    ]
    |> Command.run
         ~sandbox:Sandbox_config.needs_sandboxing
         (Ok compiler)
         ~dir:(Path.build dir)
    |> Super_context.add_rule ~dir sctx)
;;

let lib_hidden_deps ~sctx ~kind lib requires =
  let open Action_builder.O in
  let* requires = Resolve.read requires in
  Action_builder.List.concat_map requires ~f:(fun dep ->
    if Lib.equal lib dep
    then Action_builder.return []
    else (
      match Lib.Parameterised.status dep with
      | Complete ->
        let+ cm = Resolve.read (get_cm ~kind dep) in
        [ cm ]
      | Partial ->
        Code_error.raise
          "unexpected partial application"
          [ "lib", Lib.to_dyn lib; "dep", Lib.to_dyn dep ]
      | Not_parameterised ->
        let lib = dep in
        let lib_info = Lib.info dep in
        let obj_dir = Lib_info.obj_dir lib_info in
        let+ modules =
          match Lib_info.modules lib_info ~for_ with
          | External opt_modules -> Action_builder.return opt_modules
          | Local ->
            let local_lib = Lib.Local.of_lib_exn lib in
            let+ modules =
              Action_builder.of_memo
                (Dir_contents.modules_of_local_lib sctx local_lib ~for_)
            in
            Some (Modules.With_vlib.modules modules)
        in
        (match modules with
         | None -> []
         | Some modules ->
           Modules.With_vlib.fold_no_vlib_with_aliases
             modules
             ~init:[]
             ~normal:(fun module_ acc ->
               match Obj_dir.Module.cm_file obj_dir module_ ~kind:(Ocaml Cmi) with
               | None -> acc
               | Some cmi -> cmi :: acc)
             ~alias:(fun _group acc -> acc))))
  >>| Dep.Set.of_files
;;

let apply_module_name module_ args =
  let name = Module_name.Unique.to_name ~loc:Loc.none (Module.obj_name module_) in
  Parameterised_name.to_module_name { name; args }
;;

let build_modules ~sctx ~obj_dir ~modules_obj_dir ~dep_graph ~mode ~requires ~lib modules =
  let kind = Lib_mode.Cm_kind.Ocaml (Mode.cm_kind mode) in
  let ext = Lib_mode.Cm_kind.ext kind in
  let cm_args = Lib.Parameterised.arguments lib |> Resolve.List.map ~f:(get_cm ~kind) in
  let* { Lib_config.ext_obj; _ } =
    let+ ocaml = Super_context.context sctx |> Context.ocaml in
    ocaml.lib_config
  in
  let lib_hidden_deps =
    Action_builder.memoize "lib-hidden-deps" (lib_hidden_deps ~sctx ~kind lib requires)
  in
  let* args =
    (* The main module names of applied arguments is required
       because it's used in the instantiated filenames.
       If we are instantiating a library, then the existence
       of these module names has already been checked and the
       resolve can't fail. *)
    Resolve.read_memo @@ Lib.Parameterised.applied_modules lib
  in
  Memo.List.fold_left modules ~init:Module_name.Map.empty ~f:(fun acc module_ ->
    let instance =
      let module_cm = Obj_dir.Module.obj_file modules_obj_dir module_ ~kind ~ext in
      let module_cmi =
        Obj_dir.Module.cm_file_exn modules_obj_dir module_ ~kind:(Ocaml Cmi)
      in
      let unique_name = apply_module_name module_ args in
      let target = obj_file ~obj_dir ~kind ~ext unique_name in
      let o_target =
        match mode with
        | Native -> Some (obj_file ~obj_dir ~kind ~ext:ext_obj unique_name)
        | Byte -> None
      in
      let hidden_targets =
        match mode with
        | Byte -> []
        | Native -> [ obj_file ~obj_dir ~kind ~ext:ext_obj unique_name ]
      in
      let hidden_deps =
        let open Action_builder.O in
        let* lib_hidden_deps = lib_hidden_deps in
        let+ deps =
          let+ module_deps = Dep_graph.deps_of dep_graph module_ in
          List.map module_deps ~f:(fun module_ ->
            apply_module_name module_ args
            |> obj_file ~obj_dir ~kind ?ext:None
            |> Path.build)
        in
        Dep.Set.union lib_hidden_deps (Dep.Set.of_files (module_cmi :: deps))
      in
      { module_ = module_cm
      ; args = cm_args
      ; requires
      ; target
      ; o_target
      ; hidden_targets
      ; hidden_deps
      }
    in
    let+ () = build_instance ~sctx ~obj_dir ~mode instance in
    Module_name.Map.add_exn acc (Module.name module_) instance)
;;

let dep_graph ~ocaml_version ~preprocess ~obj_dir ~modules impl_only =
  let pp_map =
    Staged.unstage
    @@ Pp_spec.pped_modules_map
         (Dune_lang.Preprocess.Per_module.without_instrumentation preprocess)
         ocaml_version
  in
  let per_module =
    List.fold_left impl_only ~init:Module_name.Unique.Map.empty ~f:(fun acc module_ ->
      let module_name_unique = Module.obj_name module_ in
      let deps =
        let open Action_builder.O in
        let module_ = pp_map module_ in
        let+ deps =
          Dep_rules.read_immediate_deps_of module_ ~modules ~obj_dir ~ml_kind:Impl ~for_
        in
        let local_open = Modules.With_vlib.alias_for modules module_ in
        local_open @ deps
      in
      Module_name.Unique.Map.add_exn acc module_name_unique deps)
  in
  Dep_graph.make ~dir:(Obj_dir.dir obj_dir) ~per_module
;;

let obj_dir_for_dep_rules dir =
  Obj_dir.make_lib
    ~dir
    ~has_private_modules:false
    ~private_lib:false
    (Lib_name.Local.of_string "deps")
;;

let instantiate ~sctx lib =
  let ctx = Super_context.context sctx in
  let build_dir = Context.build_dir ctx in
  let* ocaml = Context.ocaml ctx in
  let ext_lib = ocaml.lib_config.ext_lib in
  let lib_info = Lib.info lib in
  let modules_obj_dir = Lib_info.obj_dir lib_info in
  let* deps_obj_dir, modules =
    match Lib_info.modules lib_info ~for_ with
    | External None -> Code_error.raise "library has no modules" [ "lib", Lib.to_dyn lib ]
    | External (Some modules) ->
      let dir = Lib.Parameterised.dir ~build_dir lib in
      Memo.return (obj_dir_for_dep_rules dir, modules)
    | Local ->
      let local_lib = Lib.Local.of_lib_exn lib in
      let+ modules = Dir_contents.modules_of_local_lib sctx local_lib ~for_ in
      let modules_obj_dir = Lib_info.obj_dir (Lib.Local.info local_lib) in
      modules_obj_dir, Modules.With_vlib.modules modules
  in
  let impl_only = Modules.With_vlib.impl_only modules in
  let dep_graph =
    dep_graph
      ~ocaml_version:ocaml.version
      ~preprocess:(Lib_info.preprocess ~for_ lib_info)
      ~obj_dir:deps_obj_dir
      ~modules
      impl_only
  in
  let* requires =
    Lib.closure ~linking:true [ lib ] ~for_
    |> Resolve.Memo.map
         ~f:(List.map ~f:(Lib.Parameterised.for_instance ~build_dir ~ext_lib))
  in
  let lib = Lib.Parameterised.for_instance ~build_dir ~ext_lib lib in
  let obj_dir = Lib_info.obj_dir (Lib.info lib) |> Obj_dir.as_local_exn in
  let top_sorted_modules = Dep_graph.top_closed_implementations dep_graph impl_only in
  Memo.parallel_iter Ocaml.Mode.all ~f:(fun mode ->
    let* modules =
      build_modules
        ~sctx
        ~obj_dir
        ~modules_obj_dir
        ~dep_graph
        ~mode
        ~requires
        ~lib
        impl_only
    in
    build_archive ~sctx ~mode ~obj_dir ~lib ~top_sorted_modules ~modules)
;;

let resolve_instantiation scope instance_name =
  let db = Scope.libs scope in
  let rec go { Parameterised_name.name; args } =
    Lib.DB.find db name
    >>= function
    | None -> Code_error.raise "library not found" []
    | Some lib ->
      Memo.List.map ~f:go args
      >>| List.map ~f:(fun arg -> Loc.none, arg)
      >>| Lib.Parameterised.instantiate
            ~loc:Loc.none
            ~from:`depends
            lib
            ~parent_parameters:[]
  in
  go (Parameterised_name.of_string instance_name) |> Resolve.Memo.read_memo
;;

let external_dep_rules ~sctx ~dir ~scope lib_name =
  let* lib =
    Lib.DB.find (Scope.libs scope) (Lib_name.of_string lib_name)
    >>| function
    | None -> Code_error.raise "not found" [ "lib", Dyn.string lib_name ]
    | Some lib -> lib
  in
  match Lib_info.modules (Lib.info lib) ~for_ with
  | Local -> Memo.return ()
  | External None -> Code_error.raise "library has no modules" [ "lib", Lib.to_dyn lib ]
  | External (Some modules) ->
    let+ (_ : Dep_graph.Ml_kind.t) =
      Dep_rules.rules
        ~sctx
        ~sandbox:Sandbox_config.no_special_requirements
        ~dir
        ~obj_dir:(obj_dir_for_dep_rules dir)
        ~impl:Virtual_rules.no_implements
        ~for_
        ~modules
    in
    ()
;;

let has_rules fn =
  Memo.return
    (Build_config.Gen_rules.make
       ~directory_targets:Path.Build.Map.empty
       (Rules.collect_unit fn))
;;

let find_scope ~sctx encoded_scope =
  let project_root = Parameterised_name.Scope.decode encoded_scope in
  let ctx = Super_context.context sctx in
  let dir =
    match project_root with
    | None -> Context.build_dir ctx
    | Some dir ->
      let build_context = Context.build_context ctx in
      Path.Build.append_source build_context.build_dir dir
  in
  Scope.DB.find_by_dir dir
;;

let gen_rules ~sctx ~dir rest =
  match rest with
  | [] | [ _ ] ->
    Memo.return
      (Build_config.Gen_rules.make
         ~build_dir_only_sub_dirs:
           (Build_config.Gen_rules.Build_only_sub_dirs.singleton ~dir Subdir_set.all)
         (Memo.return Rules.empty))
  | [ scope; lib_name ] ->
    let* scope = find_scope ~sctx scope in
    has_rules @@ fun () -> external_dep_rules ~sctx ~dir ~scope lib_name
  | [ scope; _lib_name; instance_name ] when not (String.equal instance_name ".deps.objs")
    ->
    let* scope = find_scope ~sctx scope in
    has_rules
    @@ fun () ->
    let* lib = resolve_instantiation scope instance_name in
    instantiate ~sctx lib
  | _ ->
    Memo.return
      (Build_config.Gen_rules.redirect_to_parent Build_config.Gen_rules.Rules.empty)
;;
