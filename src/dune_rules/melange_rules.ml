open Import
module CC = Compilation_context

let melange_js_includes ~melange_stanza_dir ~target ~requires_link ~scope =
  let open Resolve.Memo.O in
  Some
    (Command.Args.memo
       (Resolve.Memo.args
          (let+ (libs : Lib.t list) = requires_link in
           let project = Scope.project scope in
           let deps_of_lib (lib : Lib.t) =
             let lib = Lib.Local.of_lib_exn lib in
             let info = Lib.Local.info lib in
             let lib_dir = Lib_info.src_dir info in
             let dst_dir =
               Melange.lib_output_dir ~melange_stanza_dir ~lib_dir ~target
             in
             [ (let dir = Path.build dst_dir in
                Glob.of_string_exn Loc.none ("*" ^ Melange.js_ext)
                |> File_selector.of_glob ~dir |> Dep.file_selector)
             ]
             |> Dep.Set.of_list
           in
           let deps libs = Dep.Set.union_map libs ~f:deps_of_lib in
           Command.Args.S
             [ Lib_flags.L.include_flags ~project libs Melange
             ; Hidden_deps (deps libs)
             ])))

let build_js ?loc ~pkg_name ~module_system ~dst_dir ~obj_dir ~sctx ~build_dir
    ~melange_js_includes m =
  let cm_kind = Lib_mode.Cm_kind.Melange Cmj in
  let mode = Lib_mode.of_cm_kind cm_kind in
  let open Memo.O in
  let* compiler =
    let+ compiler =
      (* TODO loc should come from the mode field in the dune file *)
      Super_context.resolve_program sctx ~loc:None ~dir:build_dir
        ~hint:"opam install melange" "melc"
    in
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
    match melange_js_includes with
    | None -> Command.Args.As []
    | Some args -> Command.Args.as_any args
  in
  Super_context.add_rule sctx ~dir:build_dir ?loc
    (Command.run ~dir:(Path.build build_dir) (Ok compiler)
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
  let lib_cctx ~sctx ~obj_dir ~modules ~expander ~scope ~compile_info ~package =
    let flags = Ocaml_flags.empty in
    let vimpl =
      (* original impl in Lib_rules uses Virtual_rules.impl, will this break with virtual libs? *)
      None
    in
    let modules = Vimpl.impl_modules vimpl modules in
    let requires_compile = Lib.Compile.direct_requires compile_info in
    let requires_link = Lib.Compile.requires_link compile_info in
    let js_of_ocaml = None in
    (* modes and pp are not passed, not sure if this will cause issues *)
    Compilation_context.create () ~super_context:sctx ~expander ~scope ~obj_dir
      ~modules ~flags ~requires_compile ~requires_link
      ~opaque:Inherit_from_settings ~js_of_ocaml ~package ?vimpl
      ~modes:
        { ocaml = { byte = None; native = None }
        ; melange = Some (Requested Loc.none)
        }
  in
  let rules (mel : Melange_stanzas.Emit.t) ~sctx ~melange_stanza_dir ~scope
      ~expander =
    let open Memo.O in
    let build_dir = (Super_context.context sctx).build_dir in
    let requires_link =
      Memo.Lazy.force (Lib.Compile.requires_link all_libs_compile_info)
    in
    let* () =
      Memo.List.iter mel.entries ~f:(function
        | Folder _ -> Memo.return ()
        | Module name ->
          (* Module.generated ~src_dir:(Path.build melange_stanza_dir) m *)
          let m =
            let basename = String.uncapitalize (Module_name.to_string name) in
            let src_dir = Path.build melange_stanza_dir in
            let source =
              let impl =
                Module.File.make Dialect.ocaml
                  (* TODO: what about .re files *)
                  (Path.relative src_dir (basename ^ ".ml"))
              in
              Module.Source.make ~impl name
            in
            Module.of_source ~visibility:Private ~kind:Impl source
          in
          let obj_dir =
            (* Maybe we should have a separate objdir for melange objs, .mobjs? *)
            Obj_dir.make_exe ~dir:melange_stanza_dir ~name:mel.target
          in
          let modules_group = Modules.singleton m in
          let* cctx =
            lib_cctx ~sctx ~obj_dir ~modules:modules_group ~expander ~scope
              ~compile_info:all_libs_compile_info ~package:mel.package
          in
          let pkg_name = Option.map mel.package ~f:Package.name in
          let dst_dir = Path.Build.relative melange_stanza_dir mel.target in
          let loc = mel.loc in
          let melange_js_includes =
            melange_js_includes ~melange_stanza_dir ~target:mel.target
              ~requires_link ~scope
          in
          (* TODO: build_all should go outside Memo.List.iter, or maybe call a version of build_cm but without cctx *)
          let* () = Module_compilation.build_all cctx in
          build_js ~loc ~pkg_name ~module_system:mel.module_system ~dst_dir
            ~obj_dir ~sctx ~build_dir ~melange_js_includes m)
    in
    let* requires_link = requires_link in
    let* requires_link = Resolve.read_memo requires_link in
    Memo.List.iter requires_link ~f:(fun lib ->
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
                  "The library %s is used by a melange.emit stanza but the \
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
        let pkg_name = Lib_info.package info in
        let requires_link =
          Memo.Lazy.force (Lib.Compile.requires_link lib_compile_info)
        in
        let melange_js_includes =
          melange_js_includes ~melange_stanza_dir ~target:mel.target
            ~requires_link ~scope
        in
        Memo.parallel_iter source_modules
          ~f:
            (build_js ~pkg_name ~module_system:mel.module_system ~dst_dir
               ~obj_dir ~sctx ~build_dir ~melange_js_includes))
  in
  rules mel ~sctx ~melange_stanza_dir ~scope ~expander
