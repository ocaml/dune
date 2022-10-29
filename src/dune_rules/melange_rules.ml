open Import

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
            (Module_compilation.build_melange_js ~pkg_name
               ~module_system:mel.module_system ~dst_dir ~cctx))
  in
  rules mel ~sctx ~melange_stanza_dir ~scope ~expander
