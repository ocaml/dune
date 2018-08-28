open! Stdune
open Import
open Build.O
open Dune_file
open! No_io

module SC = Super_context

module Gen (P : Install_rules.Params) = struct
  module Odoc = Odoc.Gen(P)

  let sctx = P.sctx
  let ctx = SC.context sctx

  let opaque = SC.opaque sctx

  (* +-----------------------------------------------------------------+
     | Library stuff                                                   |
     +-----------------------------------------------------------------+ *)

  let msvc_hack_cclibs =
    List.map ~f:(fun lib ->
      let lib =
        match String.drop_prefix lib ~prefix:"-l" with
        | None -> lib
        | Some l -> l ^ ".lib"
      in
      Option.value ~default:lib (String.drop_prefix ~prefix:"-l" lib))

  let build_lib (lib : Library.t) ~scope ~flags ~dir ~obj_dir ~mode
        ~top_sorted_modules ~modules =
    Option.iter (Context.compiler ctx mode) ~f:(fun compiler ->
      let target = Library.archive lib ~dir ~ext:(Mode.compiled_lib_ext mode) in
      let stubs_flags =
        if not (Library.has_stubs lib) then
          []
        else
          let stubs_name = Lib_name.Local.to_string lib.name ^ "_stubs" in
          match mode with
          | Byte -> ["-dllib"; "-l" ^ stubs_name; "-cclib"; "-l" ^ stubs_name]
          | Native -> ["-cclib"; "-l" ^ stubs_name]
      in
      let map_cclibs =
        (* https://github.com/ocaml/dune/issues/119 *)
        if ctx.ccomp_type = "msvc" then
          msvc_hack_cclibs
        else
          fun x -> x
      in
      let artifacts ~ext modules =
        List.map modules ~f:(Module.obj_file ~obj_dir ~ext)
      in
      let obj_deps =
        Build.paths (artifacts modules ~ext:(Cm_kind.ext (Mode.cm_kind mode)))
      in
      let obj_deps =
        match mode with
        | Byte   -> obj_deps
        | Native ->
          obj_deps >>>
          Build.paths (artifacts modules ~ext:ctx.ext_obj)
      in
      SC.add_rule sctx
        (obj_deps
         >>>
         Build.fanout4
           (top_sorted_modules >>^artifacts ~ext:(Cm_kind.ext (Mode.cm_kind mode)))
           (SC.expand_and_eval_set sctx ~scope ~dir lib.c_library_flags
              ~standard:(Build.return []))
           (Ocaml_flags.get flags mode)
           (SC.expand_and_eval_set sctx ~scope ~dir lib.library_flags
              ~standard:(Build.return []))
         >>>
         Build.run ~context:ctx (Ok compiler)
           [ Dyn (fun (_, _, flags, _) -> As flags)
           ; A "-a"; A "-o"; Target target
           ; As stubs_flags
           ; Dyn (fun (_, cclibs, _, _) -> Arg_spec.quote_args "-cclib" (map_cclibs cclibs))
           ; Dyn (fun (_, _, _, library_flags) -> As library_flags)
           ; As (match lib.kind with
               | Normal -> []
               | Ppx_deriver | Ppx_rewriter -> ["-linkall"])
           ; Dyn (fun (cm_files, _, _, _) -> Deps cm_files)
           ; Hidden_targets
               (match mode with
                | Byte -> []
                | Native -> [Library.archive lib ~dir ~ext:ctx.ext_lib])
           ]))

  (* If the compiler reads the cmi for module alias even with
     [-w -49 -no-alias-deps], we must sandbox the build of the
     alias module since the modules it references are built after. *)
  let alias_module_build_sandbox =
    Ocaml_version.always_reads_alias_cmi ctx.version

  let build_alias_module (m : Module.t) ~main_module_name ~modules ~cctx
        ~dynlink ~js_of_ocaml =
    let file =
      match m.impl with
      | Some f -> f
      | None -> Option.value_exn m.intf
    in
    SC.add_rule sctx
      (Build.return
         (Module.Name.Map.values (Module.Name.Map.remove modules m.name)
          |> List.map ~f:(fun (m : Module.t) ->
            sprintf "(** @canonical %s.%s *)\n\
                     module %s = %s\n"
              (Module.Name.to_string main_module_name)
              (Module.Name.to_string m.name)
              (Module.Name.to_string m.name)
              (Module.Name.to_string (Module.real_unit_name m))
          )
          |> String.concat ~sep:"\n")
       >>> Build.write_file_dyn file.path);
    let cctx = Compilation_context.for_alias_module cctx in
    Module_compilation.build_module cctx m
      ~js_of_ocaml
      ~dynlink
      ~sandbox:alias_module_build_sandbox
      ~dep_graphs:(Ocamldep.Dep_graphs.dummy m)

  let build_c_file (lib : Library.t) ~scope ~dir ~includes (src, dst) =
    SC.add_rule sctx
      (SC.expand_and_eval_set sctx ~scope ~dir lib.c_flags
         ~standard:(Build.return (Context.cc_g ctx))
       >>>
       Build.run ~context:ctx
         (* We have to execute the rule in the library directory as
            the .o is produced in the current directory *)
         ~dir:(Path.parent_exn src)
         (Ok ctx.ocamlc)
         [ As (Utils.g ())
         ; includes
         ; Dyn (fun c_flags -> Arg_spec.quote_args "-ccopt" c_flags)
         ; A "-o"; Target dst
         ; Dep src
         ]);
    dst

  let build_cxx_file (lib : Library.t) ~scope ~dir ~includes (src, dst) =
    let open Arg_spec in
    let output_param =
      if ctx.ccomp_type = "msvc" then
        [Concat ("", [A "/Fo"; Target dst])]
      else
        [A "-o"; Target dst]
    in
    SC.add_rule sctx
      (SC.expand_and_eval_set sctx ~scope ~dir lib.cxx_flags
         ~standard:(Build.return (Context.cc_g ctx))
       >>>
       Build.run ~context:ctx
         (* We have to execute the rule in the library directory as
            the .o is produced in the current directory *)
         ~dir:(Path.parent_exn src)
         (SC.resolve_program ~loc:None sctx ctx.c_compiler)
         ([ S [A "-I"; Path ctx.stdlib_dir]
          ; As (SC.cxx_flags sctx)
          ; includes
          ; Dyn (fun cxx_flags -> As cxx_flags)
          ] @ output_param @
          [ A "-c"; Dep src
          ]));
    dst

  let ocamlmklib (lib : Library.t) ~dir ~scope ~o_files ~sandbox ~custom
        ~targets =
    SC.add_rule sctx ~sandbox
      (SC.expand_and_eval_set sctx ~scope ~dir
         lib.c_library_flags ~standard:(Build.return [])
       >>>
       Build.run ~context:ctx
         (Ok ctx.ocamlmklib)
         [ As (Utils.g ())
         ; if custom then A "-custom" else As []
         ; A "-o"
         ; Path (Path.relative dir (sprintf "%s_stubs"
                                      (Lib_name.Local.to_string lib.name)))
         ; Deps o_files
         ; Dyn (fun cclibs ->
             (* https://github.com/ocaml/dune/issues/119 *)
             if ctx.ccomp_type = "msvc" then
               let cclibs = msvc_hack_cclibs cclibs in
               Arg_spec.quote_args "-ldopt" cclibs
             else
               As cclibs
           )
         ; Hidden_targets targets
         ])

  let build_self_stubs lib ~scope ~dir ~o_files =
    let static = Library.stubs_archive lib ~dir ~ext_lib:ctx.ext_lib in
    let dynamic = Library.dll lib ~dir ~ext_dll:ctx.ext_dll in
    let modes =
      Mode_conf.Set.eval lib.modes
        ~has_native:(Option.is_some ctx.ocamlopt) in
    let ocamlmklib = ocamlmklib lib ~scope ~dir ~o_files in
    if modes.native &&
       modes.byte   &&
       Dynlink_supported.get lib.dynlink ctx.supports_shared_libraries
    then begin
      (* If we build for both modes and support dynlink, use a
         single invocation to build both the static and dynamic
         libraries *)
      ocamlmklib ~sandbox:false ~custom:false ~targets:[static; dynamic]
    end else begin
      ocamlmklib ~sandbox:false ~custom:true ~targets:[static];
      (* We can't tell ocamlmklib to build only the dll, so we
         sandbox the action to avoid overriding the static archive *)
      ocamlmklib ~sandbox:true ~custom:false ~targets:[dynamic]
    end

  let build_stubs lib ~dir ~scope ~requires ~dir_contents =
    let all_dirs = Dir_contents.dirs dir_contents in
    let h_files =
      List.fold_left all_dirs ~init:[] ~f:(fun acc dc ->
        String.Set.fold (Dir_contents.text_files dc) ~init:acc
          ~f:(fun fn acc ->
            if String.is_suffix fn ~suffix:".h" then
              Path.relative (Dir_contents.dir dc) fn :: acc
            else
              acc))
    in
    let all_dirs = Path.Set.of_list (List.map all_dirs ~f:Dir_contents.dir) in
    let resolve_name ~ext (loc, fn) =
      let p = Path.relative dir (fn ^ ext) in
      if not (match Path.parent p with
        | None -> false
        | Some p -> Path.Set.mem all_dirs p) then
        Errors.fail loc
          "File %a is not part of the current directory group. \
           This is not allowed."
          Path.pp (Path.drop_optional_build_context p)
      ;
      (p, Path.relative dir (fn ^ ctx.ext_obj))
    in
    let o_files =
      let includes =
        Arg_spec.S
          [ Hidden_deps h_files
          ; Arg_spec.of_result_map requires ~f:(fun libs ->
              S [ Lib.L.c_include_flags libs ~stdlib_dir:ctx.stdlib_dir
                ; Hidden_deps (SC.Libs.file_deps sctx libs ~ext:".h")
                ])
          ]
      in
      List.map lib.c_names ~f:(fun name ->
        build_c_file   lib ~scope ~dir ~includes (resolve_name name ~ext:".c")
      ) @ List.map lib.cxx_names ~f:(fun name ->
        build_cxx_file lib ~scope ~dir ~includes (resolve_name name ~ext:".cpp")
      )
    in
    match lib.self_build_stubs_archive with
    | Some _ -> ()
    | None -> build_self_stubs lib ~dir ~scope ~o_files

  let build_shared lib ~dir ~flags ~(ctx : Context.t) =
    Option.iter ctx.ocamlopt ~f:(fun ocamlopt ->
      let src = Library.archive lib ~dir ~ext:(Mode.compiled_lib_ext Native) in
      let dst = Library.archive lib ~dir ~ext:".cmxs" in
      let build =
        Build.dyn_paths (Build.arr (fun () ->
          [Library.archive lib ~dir ~ext:ctx.ext_lib]))
        >>>
        Ocaml_flags.get flags Native
        >>>
        Build.run ~context:ctx
          (Ok ocamlopt)
          [ Dyn (fun flags -> As flags)
          ; A "-shared"; A "-linkall"
          ; A "-I"; Path dir
          ; A "-o"; Target dst
          ; Dep src
          ]
      in
      let build =
        if Library.has_stubs lib then
          Build.path (Library.stubs_archive ~dir lib ~ext_lib:ctx.ext_lib)
          >>>
          build
        else
          build
      in
      SC.add_rule sctx build)

  let library_rules (lib : Library.t) ~dir_contents ~dir ~scope
        ~compile_info ~dir_kind =
    let obj_dir = Utils.library_object_directory ~dir lib.name in
    let requires = Lib.Compile.requires compile_info in
    let dep_kind =
      if lib.optional then Lib_deps_info.Kind.Optional else Required
    in
    let flags = SC.ocaml_flags sctx ~scope ~dir lib.buildable in
    let { Dir_contents.Library_modules.
          modules; main_module_name; alias_module } =
      Dir_contents.modules_of_library dir_contents ~name:(Library.best_name lib)
    in
    let source_modules = modules in
    (* Preprocess before adding the alias module as it doesn't need
       preprocessing *)
    let pp =
      Preprocessing.make sctx ~dir ~dep_kind ~scope
        ~preprocess:lib.buildable.preprocess
        ~preprocessor_deps:
          (SC.Deps.interpret sctx ~scope ~dir
             lib.buildable.preprocessor_deps)
        ~lint:lib.buildable.lint
        ~lib_name:(Some lib.name)
        ~dir_kind
    in
    let modules = Preprocessing.pp_modules pp modules in

    let modules =
      match alias_module with
      | None -> modules
      | Some m -> Module.Name.Map.add modules m.name m
    in

    let lib_interface_module =
      if lib.wrapped then
        Module.Name.Map.find modules main_module_name
      else
        None
    in
    let cctx =
      Compilation_context.create ()
        ~super_context:sctx
        ~scope
        ~dir
        ~dir_kind
        ~obj_dir
        ~modules
        ?alias_module
        ?lib_interface_module
        ~flags
        ~requires
        ~preprocessing:pp
        ~no_keep_locs:lib.no_keep_locs
        ~opaque
    in

    let dep_graphs = Ocamldep.rules cctx in

    let dynlink =
      Dynlink_supported.get lib.dynlink ctx.supports_shared_libraries
    in
    let js_of_ocaml = lib.buildable.js_of_ocaml in
    Module_compilation.build_modules cctx ~js_of_ocaml ~dynlink ~dep_graphs;

    Option.iter alias_module
      ~f:(build_alias_module ~main_module_name ~modules ~cctx ~dynlink
            ~js_of_ocaml);

    if Library.has_stubs lib then
      build_stubs lib ~dir ~scope ~requires ~dir_contents;

    List.iter Cm_kind.all ~f:(fun cm_kind ->
      let files =
        Module.Name.Map.fold modules ~init:Path.Set.empty ~f:(fun m acc ->
          match Module.cm_file m ~obj_dir cm_kind with
          | None -> acc
          | Some fn -> Path.Set.add acc fn)
      in
      SC.Libs.setup_file_deps_alias sctx ~dir lib ~ext:(Cm_kind.ext cm_kind)
        files);
    SC.Libs.setup_file_deps_group_alias sctx ~dir lib ~exts:[".cmi"; ".cmx"];
    SC.Libs.setup_file_deps_alias sctx ~dir lib ~ext:".h"
      (List.map lib.install_c_headers ~f:(fun header ->
         Path.relative dir (header ^ ".h"))
       |> Path.Set.of_list);

    (let modules =
       Module.Name.Map.fold modules ~init:[] ~f:(fun m acc ->
         if Module.has_impl m then
           m :: acc
         else
           acc)
     in
     let top_sorted_modules =
       Ocamldep.Dep_graph.top_closed_implementations dep_graphs.impl modules
     in
     List.iter Mode.all ~f:(fun mode ->
       build_lib lib ~scope ~flags ~dir ~obj_dir ~mode ~top_sorted_modules
         ~modules));
    (* Build *.cma.js *)
    SC.add_rules sctx (
      let src = Library.archive lib ~dir ~ext:(Mode.compiled_lib_ext Mode.Byte) in
      let target = Path.extend_basename src ~suffix:".js" in
      Js_of_ocaml_rules.build_cm cctx ~js_of_ocaml ~src ~target);

    if Dynlink_supported.By_the_os.get ctx.natdynlink_supported then
      build_shared lib ~dir ~flags ~ctx;

    Odoc.setup_library_odoc_rules lib ~requires ~modules ~dep_graphs ~scope;

    let flags =
      match alias_module with
      | None -> Ocaml_flags.common flags
      | Some m ->
        Ocaml_flags.prepend_common ["-open"; Module.Name.to_string m.name] flags
        |> Ocaml_flags.common
    in

    Sub_system.gen_rules
      { super_context = sctx
      ; dir
      ; stanza = lib
      ; scope
      ; source_modules
      ; compile_info
      };

    (cctx,
     Merlin.make ()
       ~requires:(Lib.Compile.requires compile_info)
       ~flags
       ~preprocess:(Buildable.single_preprocess lib.buildable)
       ~libname:lib.name
       ~objs_dirs:(Path.Set.singleton obj_dir))

  let rules (lib : Library.t) ~dir_contents ~dir ~scope
        ~dir_kind : Compilation_context.t * Merlin.t =
    let compile_info =
      Lib.DB.get_compile_info (Scope.libs scope) (Library.best_name lib)
        ~allow_overlaps:lib.buildable.allow_overlapping_dependencies
    in
    SC.Libs.gen_select_rules sctx compile_info ~dir;
    SC.Libs.with_lib_deps sctx compile_info ~dir
      ~f:(fun () ->
        library_rules lib ~dir_contents ~dir ~scope ~compile_info
          ~dir_kind)

end
