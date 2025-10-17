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
  let+ name = Lib.Parameterized.applied_name lib in
  let unique_name = Parameterized_name.to_module_name name in
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

let build_instance ~sctx ~obj_dir ~mode instance =
  let { module_; args; requires; target; o_target = _; hidden_targets; hidden_deps } =
    instance
  in
  let ctx = Super_context.context sctx in
  let* ocaml = Context.ocaml ctx in
  let include_flags =
    Action_builder.of_memo
    @@
    let+ requires = Resolve.read_memo requires in
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
        (Action_builder.of_memo
         @@
         let+ args = Resolve.read_memo args in
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
  let* requires = Resolve.read_memo requires in
  Memo.List.concat_map requires ~f:(fun dep ->
    match Lib.compare lib dep with
    | Eq -> Memo.return []
    | Lt | Gt ->
      (match Lib.Parameterized.status dep with
       | Complete ->
         let+ cm = Resolve.read_memo (get_cm ~kind dep) in
         [ cm ]
       | Partial ->
         Code_error.raise
           "unexpected partial application"
           [ "lib", Lib.to_dyn lib; "dep", Lib.to_dyn dep ]
       | Not_parameterized ->
         let lib = dep in
         let lib_info = Lib.info dep in
         let obj_dir = Lib_info.obj_dir lib_info in
         let+ modules =
           match Lib_info.modules lib_info with
           | External None ->
             Code_error.raise "dependency has no modules" [ "lib", Lib.to_dyn dep ]
           | External (Some modules) -> Memo.return modules
           | Local ->
             let local_lib = Lib.Local.of_lib_exn lib in
             let+ modules = Dir_contents.modules_of_local_lib sctx local_lib in
             Modules.With_vlib.modules modules
         in
         Modules.With_vlib.fold_no_vlib_with_aliases
           modules
           ~init:[]
           ~normal:(fun module_ acc ->
             match Obj_dir.Module.cm_file obj_dir module_ ~kind:(Ocaml Cmi) with
             | None -> acc
             | Some cmi -> cmi :: acc)
           ~alias:(fun _group acc -> acc)))
  >>| Dep.Set.of_files
;;

let apply_module_name module_ args =
  let name = Module_name.Unique.to_string (Module.obj_name module_) in
  Parameterized_name.to_module_name { name; args }
;;

let build_modules ~sctx ~obj_dir ~modules_obj_dir ~dep_graph ~mode ~requires ~lib modules =
  let kind = Lib_mode.Cm_kind.Ocaml (Mode.cm_kind mode) in
  let ext = Lib_mode.Cm_kind.ext kind in
  let cm_args = Lib.Parameterized.arguments lib |> Resolve.List.map ~f:(get_cm ~kind) in
  let* { Lib_config.ext_obj; _ } =
    let+ ocaml = Super_context.context sctx |> Context.ocaml in
    ocaml.lib_config
  in
  let* lib_hidden_deps = lib_hidden_deps ~sctx ~kind lib requires in
  let* args =
    (* The main module names of applied arguments is required
       because it's used in the instantiated filenames.
       If we are instantiating a library, then the existence
       of these module names has already been checked and the
       resolve can't fail. *)
    Resolve.read_memo @@ Lib.Parameterized.applied_modules lib
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
        let+ module_deps = Dep_graph.deps_of dep_graph module_ in
        let deps =
          List.map module_deps ~f:(fun module_ ->
            apply_module_name module_ args |> obj_file ~obj_dir ~kind |> Path.build)
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

let iter_modes_concurrently ~(f : Ocaml.Mode.t -> unit Memo.t) =
  let t = Mode.Dict.make_both true in
  let+ () = Memo.when_ t.byte (fun () -> f Byte)
  and+ () = Memo.when_ t.native (fun () -> f Native) in
  ()
;;

let dep_graph ~obj_dir ~modules impl_only =
  let per_module =
    List.fold_left impl_only ~init:Module_name.Unique.Map.empty ~f:(fun acc module_ ->
      let module_name_unique = Module.obj_name module_ in
      let deps =
        let open Action_builder.O in
        let+ deps =
          Dep_rules.read_immediate_deps_of module_ ~modules ~obj_dir ~ml_kind:Impl
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
  let* { Lib_config.ext_lib; _ } =
    let+ ocaml = ctx |> Context.ocaml in
    ocaml.lib_config
  in
  let lib_info = Lib.info lib in
  let modules_obj_dir = Lib_info.obj_dir lib_info in
  let* deps_obj_dir, modules =
    match Lib_info.modules lib_info with
    | External None -> Code_error.raise "library has no modules" [ "lib", Lib.to_dyn lib ]
    | External (Some modules) ->
      let dir = Path.Build.relative build_dir ".parameterized" in
      let dir = Path.Build.relative dir (Lib_name.to_string (Lib.name lib)) in
      Memo.return (obj_dir_for_dep_rules dir, modules)
    | Local ->
      let local_lib = Lib.Local.of_lib_exn lib in
      let+ modules = Dir_contents.modules_of_local_lib sctx local_lib in
      let modules_obj_dir = Lib_info.obj_dir (Lib.Local.info local_lib) in
      modules_obj_dir, Modules.With_vlib.modules modules
  in
  let impl_only = Modules.With_vlib.impl_only modules in
  let dep_graph = dep_graph ~obj_dir:deps_obj_dir ~modules impl_only in
  let* requires =
    Lib.closure ~linking:true [ lib ]
    |> Resolve.Memo.map
         ~f:(List.map ~f:(Lib.Parameterized.for_instance ~build_dir ~ext_lib))
  in
  let lib = Lib.Parameterized.for_instance ~build_dir ~ext_lib lib in
  let obj_dir = Lib_info.obj_dir (Lib.info lib) |> Obj_dir.as_local_exn in
  let top_sorted_modules = Dep_graph.top_closed_implementations dep_graph impl_only in
  iter_modes_concurrently ~f:(fun mode ->
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

let resolve_instantiation scope str =
  let db = Scope.libs scope in
  let rec go { Parameterized_name.name; args } =
    let name = Lib_name.of_string name in
    let+ lib = Lib.DB.find db name
    and+ args = Memo.List.map ~f:go args in
    match lib with
    | None -> Code_error.raise "library not found" []
    | Some lib ->
      let args = List.map args ~f:(fun arg -> Loc.none, arg) in
      Lib.Parameterized.instantiate
        ~loc:Loc.none
        (Resolve.return lib)
        args
        ~parent_parameters:[]
  in
  go (Parameterized_name.of_string str) |> Resolve.Memo.read_memo
;;

let external_dep_rules ~sctx ~dir ~scope lib_name =
  let* lib =
    Lib.DB.find (Scope.libs scope) (Lib_name.of_string lib_name)
    >>| function
    | None -> Code_error.raise "not found" [ "lib", Dyn.string lib_name ]
    | Some lib -> lib
  in
  match Lib_info.modules (Lib.info lib) with
  | Local -> Memo.return ()
  | External None -> Code_error.raise "library has no modules" [ "lib", Lib.to_dyn lib ]
  | External (Some modules) ->
    let+ _ =
      Dep_rules.rules
        ~sctx
        ~sandbox:Sandbox_config.no_special_requirements
        ~dir
        ~obj_dir:(obj_dir_for_dep_rules dir)
        ~impl:Virtual_rules.no_implements
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

let gen_rules ~sctx ~dir ~scope rest =
  match rest with
  | [] ->
    Memo.return
      (Build_config.Gen_rules.make
         ~build_dir_only_sub_dirs:
           (Build_config.Gen_rules.Build_only_sub_dirs.singleton ~dir Subdir_set.all)
         (Memo.return Rules.empty))
  | [ lib_name ] -> has_rules @@ fun () -> external_dep_rules ~sctx ~dir ~scope lib_name
  | [ _lib_name; instance_name ] when not (String.equal instance_name ".deps.objs") ->
    has_rules
    @@ fun () ->
    let* lib = resolve_instantiation scope instance_name in
    instantiate ~sctx lib
  | _ ->
    Memo.return
      (Build_config.Gen_rules.redirect_to_parent Build_config.Gen_rules.Rules.empty)
;;

type instance =
  { new_name : Module_name.t
  ; lib_name : Module_name.t
  ; args : (Module_name.t * Module_name.t) list
  }

type instances =
  | Simple of instance
  | Wrapped of Module_name.t * instance list

let instances ~sctx ~db (deps : Lib_dep.t list) =
  let open Resolve.Memo.O in
  Resolve.Memo.List.concat_map deps ~f:(function
    | Lib_dep.Direct _ | Lib_dep.Re_export _ | Lib_dep.Select _ -> Resolve.Memo.return []
    | Lib_dep.Instantiate { loc = _; lib; arguments; new_name } ->
      let+ entry_names =
        let* lib = Resolve.Memo.lift_memo @@ Lib.DB.find db lib in
        match lib with
        | None -> Resolve.Memo.return []
        | Some lib -> Root_module.entry_module_names sctx lib
      and+ args =
        Resolve.Memo.List.filter_map arguments ~f:(fun (_loc, arg_name) ->
          let* arg = Resolve.Memo.lift_memo @@ Lib.DB.find db arg_name in
          match arg with
          | None -> Resolve.Memo.return None
          | Some arg ->
            (match Lib.implements arg with
             | None -> Resolve.Memo.return None
             | Some param ->
               let* param = param in
               let+ param_name = Lib.main_module_name param
               and+ arg_name = Lib.main_module_name arg in
               (match param_name, arg_name with
                | Some param_name, Some arg_name -> Some (param_name, arg_name)
                | _ -> None)))
      in
      (match entry_names with
       | [] -> []
       | [ entry_name ] ->
         let new_name =
           match new_name with
           | None -> entry_name
           | Some new_name -> new_name
         in
         [ Simple { new_name; lib_name = entry_name; args } ]
       | _ :: _ :: _ ->
         let instances =
           List.map entry_names ~f:(fun name ->
             { new_name = name; lib_name = name; args })
         in
         (match new_name with
          | None -> List.map ~f:(fun i -> Simple i) instances
          | Some new_name -> [ Wrapped (new_name, instances) ])))
;;

let print_instance b indent instance =
  Printf.bprintf
    b
    "\n%smodule %s = %s%s [@jane.non_erasable.instances]"
    indent
    (Module_name.to_string instance.new_name)
    (Module_name.to_string instance.lib_name)
    (String.concat ~sep:""
     @@ List.map instance.args ~f:(fun (param_name, arg_name) ->
       Printf.sprintf
         "(%s)(%s)"
         (Module_name.to_string param_name)
         (Module_name.to_string arg_name)))
;;

let print_instances b instances =
  List.iter instances ~f:(fun instances ->
    match instances with
    | Simple instance -> print_instance b "" instance
    | Wrapped (new_name, instances) ->
      Printf.bprintf b "\nmodule %s = struct" (Module_name.to_string new_name);
      List.iter instances ~f:(print_instance b "  ");
      Printf.bprintf b "\nend\n")
;;
