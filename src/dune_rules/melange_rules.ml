open Import
module CC = Compilation_context

let js_includes ~emit_stanza_dir ~target ~requires_link ~scope =
  let open Resolve.Memo.O in
  Command.Args.memo
    (Resolve.Memo.args
       (let+ (libs : Lib.t list) = requires_link in
        let project = Scope.project scope in
        let deps_of_lib (lib : Lib.t) =
          let lib = Lib.Local.of_lib_exn lib in
          let info = Lib.Local.info lib in
          let lib_dir = Lib_info.src_dir info in
          let dst_dir =
            Melange.lib_output_dir ~emit_stanza_dir ~lib_dir ~target
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
          ]))

let build_js ?loc ~pkg_name ~module_system ~dst_dir ~obj_dir ~sctx ~build_dir
    ~lib_deps_js_includes m =
  let cm_kind = Lib_mode.Cm_kind.Melange Cmj in
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
  let obj_dir =
    [ Command.Args.A "-I"; Path (Path.build (Obj_dir.melange_dir obj_dir)) ]
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
  let lib_deps_js_includes = Command.Args.as_any lib_deps_js_includes in
  Super_context.add_rule sctx ~dir:build_dir ?loc
    (Command.run ~dir:(Path.build build_dir) (Ok compiler)
       [ Command.Args.S obj_dir
       ; lib_deps_js_includes
       ; As melange_package_args
       ; A "-o"
       ; Target output
       ; Dep (Path.build src)
       ]))
  |> Memo.Option.iter ~f:Fun.id

let add_rules_for_entries ~sctx ~expander ~scope ~emit_stanza_dir ~compile_info
    ~requires_link (mel : Melange_stanzas.Emit.t) =
  Memo.List.iter mel.entries ~f:(function Module name ->
      let m =
        let basename = String.uncapitalize (Module_name.to_string name) in
        let src_dir = Path.build emit_stanza_dir in
        let source =
          let impl =
            Module.File.make Dialect.ocaml
              (* TODO: what about .re files? *)
              (Path.relative src_dir (basename ^ ".ml"))
          in
          Module.Source.make ~impl name
        in
        Module.of_source ~visibility:Private ~kind:Impl source
      in
      let obj_dir =
        (* Maybe we should have a separate objdir for melange objs, .mobjs? *)
        Obj_dir.make_exe ~dir:emit_stanza_dir ~name:mel.target
      in
      let open Memo.O in
      let* cctx =
        let flags = Ocaml_flags.empty in
        let vimpl =
          (* original impl in Lib_rules uses Virtual_rules.impl, will this break with virtual libs? *)
          None
        in
        let modules = Vimpl.impl_modules vimpl (Modules.singleton_exe m) in
        let requires_compile = Lib.Compile.direct_requires compile_info in
        let requires_link = Lib.Compile.requires_link compile_info in
        let js_of_ocaml = None in
        (* modes and pp are not passed, not sure if this will cause issues *)
        Compilation_context.create () ~super_context:sctx ~expander ~scope
          ~obj_dir ~modules ~flags ~requires_compile ~requires_link
          ~opaque:Inherit_from_settings ~js_of_ocaml ~package:mel.package ?vimpl
          ~modes:
            { ocaml = { byte = None; native = None }
            ; melange = Some (Requested Loc.none)
            }
      in
      let pkg_name = Option.map mel.package ~f:Package.name in
      let dst_dir = Path.Build.relative emit_stanza_dir mel.target in
      let loc = mel.loc in
      let lib_deps_js_includes =
        js_includes ~emit_stanza_dir ~target:mel.target ~requires_link ~scope
      in
      let build_dir = (Super_context.context sctx).build_dir in
      (* TODO: build_all should go outside Memo.List.iter, or maybe call a version of build_cm but without cctx *)
      let* () = Module_compilation.build_all cctx in
      build_js ~loc ~pkg_name ~module_system:mel.module_system ~dst_dir ~obj_dir
        ~sctx ~build_dir ~lib_deps_js_includes m)

let _add_rules_for_libraries ~scope ~emit_stanza_dir ~sctx ~requires_link
    (mel : Melange_stanzas.Emit.t) =
  Memo.List.iter requires_link ~f:(fun lib ->
      let open Memo.O in
      let lib_name = Lib.name lib in
      let* lib, lib_compile_info =
        Lib.DB.get_compile_info (Scope.libs scope) lib_name
      in
      let lib = Lib.Local.of_lib_exn lib in
      let info = Lib.Local.info lib in
      let lib_dir = Lib_info.src_dir info in
      let obj_dir = Lib_info.obj_dir info in
      let () =
        if not (Path.Build.is_descendant lib_dir ~of_:emit_stanza_dir) then
          User_error.raise
            [ Pp.textf
                "The library %s is used by a melange.emit stanza but the \
                 library folder %s is not a descendant of the stanza folder %s"
                (Lib_name.to_string (Lib_info.name info))
                (Path.Build.to_string lib_dir)
                (Path.Build.to_string emit_stanza_dir)
            ]
      in
      let dst_dir =
        Melange.lib_output_dir ~emit_stanza_dir ~lib_dir ~target:mel.target
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
      let lib_deps_js_includes =
        js_includes ~emit_stanza_dir ~target:mel.target ~requires_link ~scope
      in
      let build_dir = (Super_context.context sctx).build_dir in
      Memo.parallel_iter source_modules
        ~f:
          (build_js ~pkg_name ~module_system:mel.module_system ~dst_dir ~obj_dir
             ~sctx ~build_dir ~lib_deps_js_includes))

let gen_emit_rules ~dir_contents ~stanza_dir:emit_stanza_dir ~scope ~sctx
    ~expander (mel : Melange_stanzas.Emit.t) =
  let all_libs_compile_info =
    let dune_version = Scope.project scope |> Dune_project.dune_version in
    let pps = [] in
    Lib.DB.resolve_user_written_deps_for_exes (Scope.libs scope)
      [ (mel.loc, mel.target) ]
      mel.libraries ~pps ~dune_version
  in
  let open Memo.O in
  let requires_link =
    Memo.Lazy.force (Lib.Compile.requires_link all_libs_compile_info)
  in
  let* modules, obj_dir =
    let first_exe = first_exe exes in
    Dir_contents.ocaml dir_contents
    >>| Ml_sources.modules_and_obj_dir ~for_:(Exe { first_exe })
  in

  (* let* () = *)
  add_rules_for_entries ~sctx ~expander ~scope ~emit_stanza_dir
    ~compile_info:all_libs_compile_info ~requires_link mel
(* in *)
(* let* requires_link = requires_link in *)
(* let* requires_link = Resolve.read_memo requires_link in *)
(* add_rules_for_libraries ~scope ~emit_stanza_dir ~sctx ~requires_link mel *)
