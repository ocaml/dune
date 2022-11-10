open Import
module CC = Compilation_context

let js_includes ~sctx ~emit_stanza_dir ~target ~requires_link ~scope =
  let open Resolve.Memo.O in
  Command.Args.memo
    (Resolve.Memo.args
       (let* (libs : Lib.t list) = requires_link in
        let project = Scope.project scope in
        let deps_of_lib (lib : Lib.t) =
          let lib_name = Lib.name lib in
          let lib = Lib.Local.of_lib_exn lib in
          let info = Lib.Local.info lib in
          let lib_dir = Lib_info.src_dir info in
          let dst_dir =
            Melange.lib_output_dir ~emit_stanza_dir ~lib_dir ~target
          in
          let open Memo.O in
          let modules_group =
            Dir_contents.get sctx ~dir:lib_dir
            >>= Dir_contents.ocaml
            >>| Ml_sources.modules ~for_:(Library lib_name)
          in
          let* source_modules = modules_group >>| Modules.impl_only in
          let of_module m =
            let output =
              let name =
                Module_name.Unique.artifact_filename (Module.obj_name m)
                  ~ext:Melange.js_ext
              in
              Path.Build.relative dst_dir name
            in
            Dep.file (Path.build output)
          in
          Resolve.Memo.return
            (List.map source_modules ~f:of_module |> Dep.Set.of_list)
        in
        let* hidden_libs = Resolve.Memo.List.map libs ~f:deps_of_lib in
        let hidden_deps = Dep.Set.union_all hidden_libs in
        Resolve.Memo.return
          (Command.Args.S
             [ Lib_flags.L.include_flags ~project libs Melange
             ; Hidden_deps hidden_deps
             ])))

let build_js ~loc ~dir ~pkg_name ~module_system ~dst_dir ~obj_dir ~sctx
    ~build_dir ~lib_deps_js_includes m =
  let cm_kind = Lib_mode.Cm_kind.Melange Cmj in
  let open Memo.O in
  let* compiler =
    (* TODO loc should come from the mode field in the dune file *)
    Super_context.resolve_program sctx ~loc:None ~dir:build_dir
      ~hint:"opam install melange" "melc"
  in
  let src = Obj_dir.Module.cm_file_exn obj_dir m ~kind:cm_kind in
  let output =
    let name =
      Module_name.Unique.artifact_filename (Module.obj_name m)
        ~ext:Melange.js_ext
    in
    Path.Build.relative dst_dir name
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
  Super_context.add_rule sctx ~dir ?loc
    (Command.run ~dir:(Path.build build_dir) compiler
       [ Command.Args.S obj_dir
       ; lib_deps_js_includes
       ; As melange_package_args
       ; A "-o"
       ; Target output
       ; Dep (Path.build src)
       ])

let add_rules_for_entries ~modes ~sctx ~dir ~expander ~dir_contents ~scope
    ~compile_info (mel : Melange_stanzas.Emit.t) =
  let open Memo.O in
  (* Use "mobjs" rather than "objs" to avoid a potential conflict with a library
     of the same name *)
  let* modules, obj_dir =
    Dir_contents.ocaml dir_contents
    >>| Ml_sources.modules_and_obj_dir ~for_:(Melange { target = mel.target })
  in
  let* () = Check_rules.add_obj_dir sctx ~obj_dir in
  let* flags = Super_context.ocaml_flags sctx ~dir mel.flags in
  let requires_link = Lib.Compile.requires_link compile_info in
  let direct_requires = Lib.Compile.direct_requires compile_info in
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
  let* cctx =
    let js_of_ocaml = None in
    Compilation_context.create () ~loc:mel.loc ~super_context:sctx ~expander
      ~scope ~obj_dir ~modules ~flags ~requires_link
      ~requires_compile:direct_requires ~preprocessing:pp ~js_of_ocaml
      ~opaque:Inherit_from_settings ~package:mel.package ~modes
  in
  let pkg_name = Option.map mel.package ~f:Package.name in
  let dst_dir = Path.Build.relative dir mel.target in
  let loc = mel.loc in
  let requires_link = Memo.Lazy.force requires_link in
  let lib_deps_js_includes =
    js_includes ~sctx ~emit_stanza_dir:dir ~target:mel.target ~requires_link
      ~scope
  in
  let build_dir = (Super_context.context sctx).build_dir in
  let* () = Module_compilation.build_all cctx in
  let* () =
    Memo.parallel_iter
      (Modules.fold_no_vlib modules ~init:[] ~f:(fun x acc -> x :: acc))
      ~f:(fun m ->
        (* Should we check module kind? *)
        build_js ~dir ~loc:(Some loc) ~pkg_name ~module_system:mel.module_system
          ~dst_dir ~obj_dir ~sctx ~build_dir ~lib_deps_js_includes m)
  in
  let ctx = Super_context.context sctx in
  let stdlib_dir = ctx.Context.stdlib_dir in
  let* requires_compile = Compilation_context.requires_compile cctx in
  let* preprocess =
    Resolve.Memo.read_memo
      (Preprocess.Per_module.with_instrumentation mel.preprocess
         ~instrumentation_backend:
           (Lib.DB.instrumentation_backend (Scope.libs scope)))
  in
  Memo.return
    ( cctx
    , Merlin.make ~requires:requires_compile ~stdlib_dir ~flags ~modules
        ~preprocess ~obj_dir
        ~dialects:(Dune_project.dialects (Scope.project scope))
        ~ident:(Lib.Compile.merlin_ident compile_info)
        ~modes:`Melange_emit () )

let add_rules_for_libraries ~dir ~scope ~emit_stanza_dir ~sctx ~requires_link
    (mel : Melange_stanzas.Emit.t) =
  Memo.parallel_iter requires_link ~f:(fun lib ->
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
        js_includes ~sctx ~emit_stanza_dir ~target:mel.target ~requires_link
          ~scope
      in
      let build_dir = (Super_context.context sctx).build_dir in
      Memo.parallel_iter source_modules
        ~f:
          (build_js ~loc:None ~dir ~pkg_name ~module_system:mel.module_system
             ~dst_dir ~obj_dir ~sctx ~build_dir ~lib_deps_js_includes))

let compile_info ~modes ~scope (mel : Melange_stanzas.Emit.t) =
  let open Memo.O in
  let dune_version = Scope.project scope |> Dune_project.dune_version in
  let+ pps =
    Resolve.Memo.read_memo
      (Preprocess.Per_module.with_instrumentation mel.preprocess
         ~instrumentation_backend:
           (Lib.DB.instrumentation_backend (Scope.libs scope)))
    >>| Preprocess.Per_module.pps
  in
  Lib.DB.resolve_user_written_deps_for_exes ~modes (Scope.libs scope)
    [ (mel.loc, mel.target) ]
    (snd mel.libraries) ~pps ~dune_version

let emit_rules ~dir_contents ~dir ~scope ~sctx ~expander mel =
  let open Memo.O in
  let modes =
    { Lib_mode.Map.ocaml = { byte = None; native = None }
    ; melange = Some (Dune_file.Mode_conf.Kind.Requested Loc.none)
    }
  in
  let* compile_info =
    compile_info ~modes:(Lib_mode.Map.map ~f:Option.is_some modes) ~scope mel
  in
  let+ cctx_and_merlin =
    add_rules_for_entries ~modes ~sctx ~dir ~expander ~dir_contents ~scope
      ~compile_info mel
  and+ () =
    let* requires_link =
      Memo.Lazy.force (Lib.Compile.requires_link compile_info)
    in
    let* requires_link = Resolve.read_memo requires_link in
    add_rules_for_libraries ~dir ~scope ~emit_stanza_dir:dir ~sctx
      ~requires_link mel
  in
  cctx_and_merlin
