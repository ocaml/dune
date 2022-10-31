open Import
module CC = Compilation_context

let compiler ~sctx ~dir =
  (* TODO loc should come from the mode field in the dune file *)
  Super_context.resolve_program sctx ~loc:None ~dir ~hint:"opam install melange"
    "melc"

let build_js ~pkg_name ~module_system ~dst_dir ~cctx m =
  let cm_kind = Lib_mode.Cm_kind.Melange Cmj in
  let sctx = CC.super_context cctx in
  let obj_dir = CC.obj_dir cctx in
  let ctx = Super_context.context sctx in
  let dir = ctx.build_dir in
  let mode = Lib_mode.of_cm_kind cm_kind in
  let ml_kind = Lib_mode.Cm_kind.source cm_kind in
  let open Memo.O in
  let* compiler =
    let+ compiler = compiler ~sctx ~dir in
    Result.to_option compiler
  in
  (let open Option.O in
  let+ compiler = compiler in
  let src = Obj_dir.Module.cm_file_exn obj_dir m ~kind:cm_kind in
  let in_dir = Path.Build.relative dst_dir in
  let output =
    let name =
      Module_name.Unique.artifact_filename (Module.obj_name m)
        ~ext:Melange.js_ext
    in
    in_dir name
  in
  let obj_dirs =
    Obj_dir.all_obj_dirs obj_dir ~mode
    |> List.concat_map ~f:(fun p ->
           [ Command.Args.A "-I"; Path (Path.build p) ])
  in
  let dep_graph = Ml_kind.Dict.get (CC.dep_graphs cctx) ml_kind in
  let cmj_deps =
    Action_builder.dyn_paths_unit
      (let open Action_builder.O in
      let+ deps = Dep_graph.deps_of dep_graph m in
      List.concat_map deps ~f:(fun m ->
          if Module.has m ~ml_kind:Impl && cm_kind = Melange Cmj then
            let name =
              Module_name.Unique.artifact_filename (Module.obj_name m)
                ~ext:Melange.js_ext
            in
            [ Path.build (in_dir name) ]
          else []))
  in
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
  let melange_js_includes =
    match CC.melange_js_includes cctx with
    | None -> Command.Args.As []
    | Some args -> Command.Args.as_any args
  in
  Super_context.add_rule sctx ~dir ?loc:(CC.loc cctx)
    (let open Action_builder.With_targets.O in
    Action_builder.with_no_targets cmj_deps
    >>> Command.run ~dir:(Path.build dir) (Ok compiler)
          [ Command.Args.S obj_dirs
          ; melange_js_includes
          ; As melange_package_args
          ; A "-o"
          ; Target output
          ; Dep (Path.build src)
          ]))
  |> Memo.Option.iter ~f:Fun.id

let gen_rules ~melange_stanza_dir ~scope ~sctx ~expander
    (mel : Melange_stanzas.Emit.t) =
  let all_libs_compile_info =
    let dune_version = Scope.project scope |> Dune_project.dune_version in
    let pps = [] in
    Lib.DB.resolve_user_written_deps_for_exes (Scope.libs scope)
      [ (mel.loc, mel.target) ]
      mel.libraries ~pps ~dune_version
  in
  let lib_cctx ~sctx ~obj_dir ~modules ~expander ~scope ~compile_info =
    let flags = Ocaml_flags.empty in
    let vimpl =
      (* original impl in Lib_rules uses Virtual_rules.impl, will this break with virtual libs? *)
      None
    in
    let modules = Vimpl.impl_modules vimpl modules in
    let requires_compile = Lib.Compile.direct_requires compile_info in
    let requires_link = Lib.Compile.requires_link compile_info in
    let package = None in
    let js_of_ocaml = None in
    let melange =
      Melange.In_context.make ~target:mel.target ~melange_stanza_dir
    in
    (* modes and pp are not passed, not sure if this will cause issues *)
    Compilation_context.create () ~super_context:sctx ~expander ~scope ~obj_dir
      ~modules ~flags ~requires_compile ~requires_link
      ~opaque:Inherit_from_settings ~js_of_ocaml ~package ~melange ?vimpl
  in
  let rules (mel : Melange_stanzas.Emit.t) ~sctx ~melange_stanza_dir ~scope
      ~expander =
    let open Memo.O in
    let* libs =
      Memo.Lazy.force (Lib.Compile.requires_link all_libs_compile_info)
    in
    let* libs = Resolve.read_memo libs in
    Memo.List.iter libs ~f:(fun lib ->
        let lib_name = Lib.name lib in
        let* lib, lib_compile_info =
          Lib.DB.get_compile_info (Scope.libs scope) lib_name
        in
        let lib = Lib.Local.of_lib_exn lib in
        let info = Lib.Local.info lib in
        let lib_dir = Lib_info.src_dir info in
        let obj_dir = Lib_info.obj_dir info in
        let () =
          if not (Path.Build.is_descendant lib_dir ~of_:melange_stanza_dir) then
            User_error.raise
              [ Pp.textf
                  "The library %s is used from a melange.emit stanza but the \
                   library folder %s is not a descendant of the stanza folder \
                   %s"
                  (Lib_name.to_string (Lib_info.name info))
                  (Path.Build.to_string lib_dir)
                  (Path.Build.to_string melange_stanza_dir)
              ]
        in
        let dst_dir =
          Melange.lib_output_dir ~melange_stanza_dir ~lib_dir ~target:mel.target
        in
        let modules_group =
          Dir_contents.get sctx ~dir:lib_dir
          >>= Dir_contents.ocaml
          >>| Ml_sources.modules ~for_:(Library lib_name)
        in
        let* source_modules = modules_group >>| Modules.impl_only in
        let* modules_group = modules_group in
        let* cctx =
          lib_cctx ~sctx ~obj_dir ~modules:modules_group ~expander ~scope
            ~compile_info:lib_compile_info
        in
        let pkg_name = Lib_info.package info in
        Memo.parallel_iter source_modules
          ~f:
            (build_js ~pkg_name ~module_system:mel.module_system ~dst_dir ~cctx))
  in
  rules mel ~sctx ~melange_stanza_dir ~scope ~expander
