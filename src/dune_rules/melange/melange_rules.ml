open Import

let ocaml_flags sctx ~dir melange =
  let open Memo.O in
  let* expander = Super_context.expander sctx ~dir in
  let* flags =
    let+ ocaml_flags =
      Super_context.env_node sctx ~dir >>= Env_node.ocaml_flags
    in
    Ocaml_flags.make_with_melange ~melange ~default:ocaml_flags
      ~eval:(Expander.expand_and_eval_set expander)
  in
  Super_context.build_dir_is_vendored dir >>| function
  | false -> flags
  | true ->
    let ocaml_version = (Super_context.context sctx).version in
    Super_context.with_vendored_flags ~ocaml_version flags

let output_of_lib ~target_dir lib =
  let info = Lib.info lib in
  match Lib_info.status info with
  | Private _ -> `Private_library_or_emit target_dir
  | Installed | Installed_private ->
    let lib_name = Lib_info.name info in
    `Public_library
      (Path.Build.L.relative target_dir
         [ "node_modules"; Lib_name.to_string lib_name ])
  | Public _ ->
    let lib_name = Lib_info.name info in
    let src_dir = Lib_info.src_dir info in
    `Public_library
      (Path.Build.L.relative target_dir
         [ "node_modules"
         ; Lib_name.to_string lib_name
         ; Path.Source.to_string (Path.drop_build_context_exn src_dir)
         ])

let make_js_name ~js_ext ~output m =
  let name = Melange.js_basename m ^ js_ext in
  match output with
  | `Public_library output_dir -> Path.Build.relative output_dir name
  | `Private_library_or_emit target_dir ->
    let dst_dir =
      Path.Build.append_source target_dir
        (Module.file m ~ml_kind:Impl
        |> Option.value_exn |> Path.as_in_build_dir_exn |> Path.Build.parent_exn
        |> Path.Build.drop_build_context_exn)
    in
    Path.Build.relative dst_dir name

let impl_only_modules_defined_in_this_lib sctx lib =
  let open Memo.O in
  let+ modules = Dir_contents.modules_of_lib sctx lib >>| Option.value_exn in
  (* for a virtual library,this will return all modules *)
  (Modules.split_by_lib modules).impl
  |> List.filter ~f:(Module.has ~ml_kind:Impl)

let cmj_glob = Glob.of_string_exn Loc.none "*.cmj"

let cmj_includes ~(requires_link : Lib.t list Resolve.t) ~scope =
  let project = Scope.project scope in
  let deps_of_lib lib =
    let info = Lib.info lib in
    let obj_dir = Lib_info.obj_dir info in
    let dir = Obj_dir.melange_dir obj_dir in
    Dep.file_selector @@ File_selector.of_glob ~dir cmj_glob
  in
  let open Resolve.O in
  Command.Args.memo @@ Resolve.args
  @@ let+ requires_link = requires_link in
     let deps = List.map requires_link ~f:deps_of_lib |> Dep.Set.of_list in
     Command.Args.S
       [ Lib_flags.L.include_flags ~project requires_link Melange
       ; Hidden_deps deps
       ]

let compile_info ~scope (mel : Melange_stanzas.Emit.t) =
  let open Memo.O in
  let dune_version = Scope.project scope |> Dune_project.dune_version in
  let+ pps =
    Resolve.Memo.read_memo
      (Preprocess.Per_module.with_instrumentation mel.preprocess
         ~instrumentation_backend:
           (Lib.DB.instrumentation_backend (Scope.libs scope)))
    >>| Preprocess.Per_module.pps
  in
  let merlin_ident = Merlin_ident.for_melange ~target:mel.target in
  Lib.DB.resolve_user_written_deps (Scope.libs scope) (`Melange_emit mel.target)
    ~allow_overlaps:mel.allow_overlapping_dependencies ~forbidden_libraries:[]
    mel.libraries ~pps ~dune_version ~merlin_ident

let js_targets_of_modules modules ~js_ext ~output =
  Modules.fold_no_vlib modules ~init:Path.Set.empty ~f:(fun m acc ->
      if Module.has m ~ml_kind:Impl then
        let target = Path.build @@ make_js_name ~js_ext ~output m in
        Path.Set.add acc target
      else acc)

let js_targets_of_libs sctx libs ~js_ext ~target_dir =
  let of_lib lib =
    let open Memo.O in
    let+ modules = impl_only_modules_defined_in_this_lib sctx lib in
    let output = output_of_lib ~target_dir lib in
    List.rev_map modules ~f:(fun m ->
        Path.build @@ make_js_name ~output ~js_ext m)
  in
  Resolve.Memo.List.concat_map libs ~f:(fun lib ->
      let open Memo.O in
      let* base = of_lib lib in
      match Lib.implements lib with
      | None -> Resolve.Memo.return base
      | Some vlib ->
        let open Resolve.Memo.O in
        let* vlib = vlib in
        let+ for_vlib = Resolve.Memo.lift_memo (of_lib vlib) in
        List.rev_append for_vlib base)

let build_js ~loc ~dir ~pkg_name ~mode ~module_system ~output ~obj_dir ~sctx
    ~includes ~js_ext m =
  let open Memo.O in
  let* compiler = Melange_binary.melc sctx ~loc:(Some loc) ~dir in
  let src = Obj_dir.Module.cm_file_exn obj_dir m ~kind:(Melange Cmj) in
  let output = make_js_name ~output ~js_ext m in
  let obj_dir = [ Command.Args.A "-I"; Path (Obj_dir.melange_dir obj_dir) ] in
  let melange_package_args =
    let pkg_name_args =
      match pkg_name with
      | None -> []
      | Some pkg_name ->
        [ "--bs-package-name"; Package.Name.to_string pkg_name ]
    in
    let js_modules_str = Melange.Module_system.to_string module_system in
    "--bs-module-type" :: js_modules_str :: pkg_name_args
  in
  Super_context.add_rule sctx ~dir ~loc ~mode
    (Command.run
       ~dir:(Path.build (Super_context.context sctx).build_dir)
       compiler
       [ Command.Args.S obj_dir
       ; Command.Args.as_any includes
       ; As melange_package_args
       ; A "-o"
       ; Target output
       ; Dep src
       ])

let setup_emit_cmj_rules ~sctx ~dir ~scope ~expander ~dir_contents
    (mel : Melange_stanzas.Emit.t) =
  let open Memo.O in
  let* compile_info = compile_info ~scope mel in
  let ctx = Super_context.context sctx in
  let f () =
    (* Use "mobjs" rather than "objs" to avoid a potential conflict with a library
       of the same name *)
    let* modules, obj_dir =
      Dir_contents.ocaml dir_contents
      >>| Ml_sources.modules_and_obj_dir ~for_:(Melange { target = mel.target })
    in
    let* () = Check_rules.add_obj_dir sctx ~obj_dir `Melange in
    let* modules, pp =
      Buildable_rules.modules_rules sctx
        (Melange
           { preprocess = mel.preprocess
           ; preprocessor_deps = mel.preprocessor_deps
           ; (* TODO still needed *)
             lint = Preprocess.Per_module.default ()
           ; (* why is this always false? *)
             empty_module_interface_if_absent = false
           })
        expander ~dir scope modules
    in
    let requires_link = Lib.Compile.requires_link compile_info in
    let* flags = ocaml_flags sctx ~dir mel.compile_flags in
    let* cctx =
      let js_of_ocaml = None in
      let direct_requires = Lib.Compile.direct_requires compile_info in
      Compilation_context.create () ~loc:mel.loc ~super_context:sctx ~expander
        ~scope ~obj_dir ~modules ~flags ~requires_link
        ~requires_compile:direct_requires ~preprocessing:pp ~js_of_ocaml
        ~opaque:Inherit_from_settings ~package:mel.package
        ~modes:
          { ocaml = { byte = None; native = None }
          ; melange = Some (Requested Loc.none)
          }
    in
    let* () = Module_compilation.build_all cctx in
    let* requires_compile = Compilation_context.requires_compile cctx in
    let preprocess =
      Preprocess.Per_module.with_instrumentation mel.preprocess
        ~instrumentation_backend:
          (Lib.DB.instrumentation_backend (Scope.libs scope))
    in
    let stdlib_dir = ctx.stdlib_dir in
    let+ () =
      let target_dir = Path.Build.relative dir mel.target in
      match mel.alias with
      | None -> Memo.return ()
      | Some alias_name ->
        let js_ext = mel.javascript_extension in
        let deps =
          js_targets_of_modules ~output:(`Private_library_or_emit target_dir)
            ~js_ext modules
          |> Action_builder.path_set
        in
        let alias = Alias.make alias_name ~dir in
        let* () = Rules.Produce.Alias.add_deps alias deps in
        (let open Action_builder.O in
        let* deps =
          Resolve.Memo.read
          @@
          let open Resolve.Memo.O in
          Compilation_context.requires_link cctx
          >>= js_targets_of_libs sctx ~js_ext ~target_dir
        in
        Action_builder.paths deps)
        |> Rules.Produce.Alias.add_deps alias
    in
    ( cctx
    , Merlin.make ~requires:requires_compile ~stdlib_dir ~flags ~modules
        ~source_dirs:Path.Source.Set.empty ~libname:None ~preprocess ~obj_dir
        ~ident:(Lib.Compile.merlin_ident compile_info)
        ~dialects:(Dune_project.dialects (Scope.project scope))
        ~modes:`Melange_emit )
  in
  Buildable_rules.with_lib_deps ctx compile_info ~dir ~f

let setup_entries_js ~sctx ~dir ~dir_contents ~scope ~compile_info ~target_dir
    ~mode (mel : Melange_stanzas.Emit.t) =
  let open Memo.O in
  (* Use "mobjs" rather than "objs" to avoid a potential conflict with a library
     of the same name *)
  let* modules, obj_dir =
    Dir_contents.ocaml dir_contents
    >>| Ml_sources.modules_and_obj_dir ~for_:(Melange { target = mel.target })
  in
  let* modules =
    let version = (Super_context.context sctx).version in
    let* preprocess =
      Resolve.Memo.read_memo
        (Preprocess.Per_module.with_instrumentation mel.preprocess
           ~instrumentation_backend:
             (Lib.DB.instrumentation_backend (Scope.libs scope)))
    in
    let pped_map =
      Staged.unstage (Preprocessing.pped_modules_map preprocess version)
    in
    Modules.map_user_written modules ~f:(fun m -> Memo.return @@ pped_map m)
  in
  let requires_link = Lib.Compile.requires_link compile_info in
  let pkg_name = Option.map mel.package ~f:Package.name in
  let loc = mel.loc in
  let js_ext = mel.javascript_extension in
  let* requires_link = Memo.Lazy.force requires_link in
  let includes = cmj_includes ~requires_link ~scope in
  let modules_for_js =
    Modules.fold_no_vlib modules ~init:[] ~f:(fun x acc ->
        if Module.has x ~ml_kind:Impl then x :: acc else acc)
  in
  let output = `Private_library_or_emit target_dir in
  let obj_dir = Obj_dir.of_local obj_dir in
  Memo.parallel_iter modules_for_js ~f:(fun m ->
      build_js ~dir ~loc ~pkg_name ~mode ~module_system:mel.module_system
        ~output ~obj_dir ~sctx ~includes ~js_ext m)

let setup_js_rules_libraries ~dir ~scope ~target_dir ~sctx ~requires_link ~mode
    (mel : Melange_stanzas.Emit.t) =
  let build_js = build_js ~sctx ~mode ~js_ext:mel.javascript_extension in
  Memo.parallel_iter requires_link ~f:(fun lib ->
      let open Memo.O in
      let lib_name = Lib.name lib in
      let* lib, lib_compile_info =
        Lib.DB.get_compile_info (Scope.libs scope) lib_name
          ~allow_overlaps:mel.allow_overlapping_dependencies
      in
      let info = Lib.info lib in
      let loc = Lib_info.loc info in
      let build_js =
        let obj_dir = Lib_info.obj_dir info in
        let pkg_name = Lib_info.package info in
        build_js ~loc ~pkg_name ~module_system:mel.module_system ~obj_dir
      in
      let* includes =
        let+ requires_link =
          Memo.Lazy.force (Lib.Compile.requires_link lib_compile_info)
        in
        cmj_includes ~requires_link ~scope
      in
      let* () =
        match Lib.implements lib with
        | None -> Memo.return ()
        | Some vlib ->
          let* vlib = Resolve.Memo.read_memo vlib in
          let* includes =
            let+ requires_link =
              Lib.Compile.for_lib
                ~allow_overlaps:mel.allow_overlapping_dependencies
                (Scope.libs scope) vlib
              |> Lib.Compile.requires_link |> Memo.Lazy.force
            in
            cmj_includes ~requires_link ~scope
          in
          let output = output_of_lib ~target_dir lib in
          impl_only_modules_defined_in_this_lib sctx vlib
          >>= Memo.parallel_iter ~f:(build_js ~dir ~output ~includes)
      in
      let output = output_of_lib ~target_dir lib in
      let* source_modules = impl_only_modules_defined_in_this_lib sctx lib in
      Memo.parallel_iter source_modules ~f:(build_js ~dir ~output ~includes))

let setup_emit_js_rules ~dir_contents ~dir ~scope ~sctx mel =
  let open Memo.O in
  let* compile_info = compile_info ~scope mel in
  let target_dir =
    Path.Build.relative (Dir_contents.dir dir_contents) mel.target
  in
  let mode =
    match mel.promote with
    | None -> Rule.Mode.Standard
    | Some p -> Promote p
  in
  let* () =
    let* requires_link =
      Lib.Compile.requires_link compile_info
      |> Memo.Lazy.force >>= Resolve.read_memo
    in
    setup_js_rules_libraries ~dir ~scope ~target_dir ~sctx ~requires_link ~mode
      mel
  in
  setup_entries_js ~sctx ~dir ~dir_contents ~scope ~compile_info ~target_dir
    ~mode mel
