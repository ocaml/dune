open! Stdune
open Import
open Build.O
open! No_io

module Buildable = Dune_file.Buildable
module Library = Dune_file.Library
module Mode_conf = Dune_file.Mode_conf

module SC = Super_context

module Gen (P : sig val sctx : Super_context.t end) = struct
  let sctx = P.sctx
  let ctx = SC.context sctx

  let opaque = SC.opaque sctx

  let msvc_hack_cclibs =
    List.map ~f:(fun lib ->
      let lib =
        match String.drop_prefix lib ~prefix:"-l" with
        | None -> lib
        | Some l -> l ^ ".lib"
      in
      Option.value ~default:lib (String.drop_prefix ~prefix:"-l" lib))

  let build_lib (lib : Library.t) ~obj_dir ~expander ~flags ~dir ~mode
        ~top_sorted_modules ~modules =
    let { Lib_config. ext_obj; ext_lib; _ } = ctx.lib_config in
    let cm_files =
      Cm_files.make_lib ~obj_dir ~ext_obj ~modules ~top_sorted_modules in
    Option.iter (Context.compiler ctx mode) ~f:(fun compiler ->
      let target = Library.archive lib ~dir ~ext:(Mode.compiled_lib_ext mode) in
      let stubs_flags =
        if not (Library.has_stubs lib) then
          []
        else
          let stubs_name = Library.stubs_name lib in
          match mode with
          | Byte -> ["-dllib"; "-l" ^ stubs_name; "-cclib"; "-l" ^ stubs_name]
          | Native -> ["-cclib"; "-l" ^ stubs_name]
      in
      let map_cclibs =
        (* https://github.com/ocaml/dune/issues/119 *)
        if ctx.ccomp_type = "msvc" then
          msvc_hack_cclibs
        else
          Fn.id
      in
      let obj_deps = Build.paths (Cm_files.unsorted_objects_and_cms cm_files ~mode) in
      let ocaml_flags = Ocaml_flags.get flags mode in
      let cclibs = Expander.expand_and_eval_set expander lib.c_library_flags
                     ~standard:(Build.return []) in
      let library_flags =
        Expander.expand_and_eval_set expander lib.library_flags
          ~standard:(Build.return []) in
      SC.add_rule ~dir sctx ~loc:lib.buildable.loc
        (Build.S.seq obj_deps
           (Command.run (Ok compiler) ~dir:(Path.build ctx.build_dir)
              [ Command.Args.dyn ocaml_flags
              ; A "-a"; A "-o"; Target target
              ; As stubs_flags
              ; Dyn (Build.S.map cclibs
                       ~f:(fun x -> Command.quote_args "-cclib" (map_cclibs x)))
              ; Command.Args.dyn library_flags
              ; As (match lib.kind with
                  | Normal -> []
                  | Ppx_deriver _ | Ppx_rewriter _ -> ["-linkall"])
              ; Dyn (
                  Cm_files.top_sorted_cms cm_files ~mode
                  |> Build.S.map ~f:(fun x -> Command.Args.Deps x))
              ; Hidden_targets
                  (match mode with
                   | Byte -> []
                   | Native ->
                     [Library.archive lib ~dir ~ext:ext_lib])
              ])))

  let build_alias_module ~loc ~alias_module ~lib_modules ~dir ~cctx =
    let vimpl = Compilation_context.vimpl cctx in
    let file = Option.value_exn (Module.file alias_module ~ml_kind:Impl) in
    let alias_file () =
      let main_module_name =
        Option.value_exn (Lib_modules.main_module_name lib_modules)
      in
      Vimpl.aliased_modules vimpl lib_modules
      |> Module.Name.Map.values
      |> List.map ~f:(fun (m : Module.t) ->
        let name = Module.Name.to_string (Module.name m) in
        sprintf "(** @canonical %s.%s *)\n\
                 module %s = %s\n"
          (Module.Name.to_string main_module_name)
          name
          name
          (Module.Name.to_string (Module.real_unit_name m)))
      |> String.concat ~sep:"\n"
    in
    SC.add_rule ~loc sctx ~dir (
      Build.arr alias_file
      >>> Build.write_file_dyn (Path.as_in_build_dir_exn file)
    );
    let cctx = Compilation_context.for_alias_module cctx in
    Module_compilation.build_module cctx alias_module
      ~dep_graphs:(Dep_graph.Ml_kind.dummy alias_module)

  let build_wrapped_compat_modules (lib : Library.t) cctx ~lib_modules =
    let wrapped_compat = Lib_modules.wrapped_compat lib_modules in
    let modules = Lib_modules.modules lib_modules in
    let wrapped = Lib_modules.wrapped lib_modules in
    let transition_message = lazy (
      match (wrapped : Wrapped.t) with
      | Simple _ -> assert false
      | Yes_with_transition r -> r)
    in
    Module.Name.Map.iteri wrapped_compat ~f:(fun name m ->
      let main_module_name =
        match Library.main_module_name lib with
        | This (Some mmn) -> Module.Name.to_string mmn
        | _ -> assert false
      in
      let contents =
        let name = Module.Name.to_string name in
        let hidden_name = sprintf "%s__%s" main_module_name name in
        let real_name = sprintf "%s.%s" main_module_name name in
        sprintf {|[@@@deprecated "%s. Use %s instead."] include %s|}
          (Lazy.force transition_message) real_name hidden_name
      in
      let source_path = Option.value_exn (Module.file m ~ml_kind:Impl) in
      let loc = lib.buildable.loc in
      Build.return contents
      >>> Build.write_file_dyn (Path.as_in_build_dir_exn source_path)
      |> SC.add_rule sctx ~loc ~dir:(Compilation_context.dir cctx)
    );
    let dep_graphs =
      Dep_graph.Ml_kind.wrapped_compat ~modules ~wrapped_compat in
    let cctx = Compilation_context.for_wrapped_compat cctx wrapped_compat in
    Module.Name.Map.iter wrapped_compat
      ~f:(Module_compilation.build_module cctx ~dep_graphs)

  let build_c_file (lib : Library.t) ~dir ~expander ~includes (loc, src, dst) =
    let c_flags = (SC.c_flags sctx ~dir ~expander ~flags:lib.c_flags).c in
    SC.add_rule sctx ~loc ~dir
      (
        let src = Path.build (C.Source.path src) in
        Command.run
          (* We have to execute the rule in the library directory as
             the .o is produced in the current directory *)
          ~dir:(Path.build dir)
          (Ok ctx.ocamlc)
          [ A "-g"
          ; includes
          ; Dyn (
              Build.S.map c_flags ~f:(fun x -> Command.quote_args "-ccopt" x))
          ; A "-o"; Target dst
          ; Dep src
          ]);
    dst

  let build_cxx_file
        (lib : Library.t) ~dir ~expander ~includes (loc, src, dst) =
    let output_param =
      if ctx.ccomp_type = "msvc" then
        [Command.Args.Concat ("", [A "/Fo"; Target dst])]
      else
        [A "-o"; Target dst]
    in
    let cxx_flags = (SC.c_flags sctx ~dir ~expander ~flags:lib.c_flags).cxx in
    SC.add_rule sctx ~loc ~dir (
      let src = Path.build (C.Source.path src) in
      Command.run
        (* We have to execute the rule in the library directory as
           the .o is produced in the current directory *)
        ~dir:(Path.build dir)
        (SC.resolve_program ~loc:None ~dir sctx ctx.c_compiler)
        ([ Command.Args.S [A "-I"; Path ctx.stdlib_dir]
         ; includes
         ; Command.Args.dyn cxx_flags
         ] @ output_param @
         [ A "-c"; Dep src
         ]));
    dst

  let ocamlmklib (lib : Library.t) ~dir ~expander ~o_files ~sandbox ~custom
        ~targets =
    SC.add_rule sctx ~sandbox ~dir
      ~loc:lib.buildable.loc
      (let cclibs_args = Expander.expand_and_eval_set expander
                           lib.c_library_flags ~standard:(Build.return [])
       in
       Command.run ~dir:(Path.build ctx.build_dir)
         (Ok ctx.ocamlmklib)
         [ A "-g"
         ; if custom then A "-custom" else As []
         ; A "-o"
         ; Path (Path.build (Library.stubs lib ~dir))
         ; Deps o_files
         ; Dyn (Build.S.map cclibs_args ~f:(fun cclibs ->
             (* https://github.com/ocaml/dune/issues/119 *)
             if ctx.ccomp_type = "msvc" then
               let cclibs = msvc_hack_cclibs cclibs in
               Command.quote_args "-ldopt" cclibs
             else
               As cclibs
           ))
         ; Hidden_targets targets
         ])

  let build_self_stubs lib ~expander ~dir ~o_files =
    let { Lib_config. ext_lib; ext_dll; has_native; _ } = ctx.lib_config in
    let static = Library.stubs_archive lib ~dir ~ext_lib in
    let dynamic = Library.dll lib ~dir ~ext_dll in
    let modes = Mode_conf.Set.eval lib.modes ~has_native in
    let ocamlmklib = ocamlmklib lib ~expander ~dir ~o_files in
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

  let build_o_files lib ~(c_sources : C.Sources.t)
        ~dir ~expander ~requires ~dir_contents =
    let all_dirs = Dir_contents.dirs dir_contents in
    let h_files =
      List.fold_left all_dirs ~init:[] ~f:(fun acc dc ->
        String.Set.fold (Dir_contents.text_files dc) ~init:acc
          ~f:(fun fn acc ->
            if String.is_suffix fn ~suffix:C.header_ext then
              Path.relative (Path.build (Dir_contents.dir dc)) fn :: acc
            else
              acc))
    in
    let includes =
      Command.Args.S [ Hidden_deps (Dep.Set.of_files h_files)
        ; Command.of_result_map requires ~f:(fun libs ->
            S [ Lib.L.c_include_flags libs ~stdlib_dir:ctx.stdlib_dir
              ; Hidden_deps (
                  Lib_file_deps.deps libs
                    ~groups:[Lib_file_deps.Group.Header])
              ])
        ]
    in
    let build_x_files build_x files =
      String.Map.to_list files
      |> List.map ~f:(fun (obj, (loc, src)) ->
        let dst = Path.Build.relative dir (obj ^ ctx.lib_config.ext_obj) in
        build_x lib ~dir ~expander ~includes (loc, src, dst)
      )
    in
    let { C.Kind.Dict. c; cxx } = C.Sources.split_by_kind c_sources in
    build_x_files build_c_file c
    @ build_x_files build_cxx_file cxx
    |> List.map ~f:Path.build

  let build_stubs lib ~dir ~expander ~requires ~dir_contents
        ~vlib_stubs_o_files =
    let lib_o_files =
      if Library.has_stubs lib then
        let c_sources = Dir_contents.c_sources_of_library
                          dir_contents ~name:(Library.best_name lib) in
        build_o_files lib ~dir ~expander ~requires ~dir_contents ~c_sources
      else
        []
    in
    match vlib_stubs_o_files @ lib_o_files with
    | [] -> ()
    | o_files -> build_self_stubs lib ~dir ~expander ~o_files

  let build_shared lib ~dir ~flags ~(ctx : Context.t) =
    Option.iter ctx.ocamlopt ~f:(fun ocamlopt ->
      let ext_lib = ctx.lib_config.ext_lib in
      let src =
        let ext = Mode.compiled_lib_ext Native in
        Path.build (Library.archive lib ~dir ~ext)
      in
      let dst =
        let ext = Mode.plugin_ext Native in
        Library.archive lib ~dir ~ext
      in
      let build =
        Build.S.seq (Build.dyn_paths (Build.arr (fun () -> [
            Path.build (Library.archive lib ~dir ~ext:ext_lib)
          ])))
          (Command.run ~dir:(Path.build ctx.build_dir)
             (Ok ocamlopt)
             [ Command.Args.dyn (Ocaml_flags.get flags Native)
             ; A "-shared"; A "-linkall"
             ; A "-I"; Path (Path.build dir)
             ; A "-o"; Target dst
             ; Dep src
             ])
      in
      let build =
        if Library.has_stubs lib then
          Build.path (Path.build (Library.stubs_archive ~dir lib ~ext_lib))
          >>>
          build
        else
          build
      in
      SC.add_rule sctx build ~dir)

  let setup_build_archives (lib : Dune_file.Library.t)
        ~wrapped_compat ~cctx ~(dep_graphs : Dep_graph.Ml_kind.t)
        ~expander =
    let dir = Compilation_context.dir cctx in
    let obj_dir = Compilation_context.obj_dir cctx in
    let flags = Compilation_context.flags cctx in
    let modules = Compilation_context.modules cctx in
    let js_of_ocaml = lib.buildable.js_of_ocaml in
    let vimpl = Compilation_context.vimpl cctx in
    let { Lib_config. ext_obj; has_native; natdynlink_supported; _ } =
      ctx.lib_config in
    let modules =
      match lib.stdlib with
      | Some { exit_module = Some name; _ } -> begin
          match Module.Name.Map.find modules name with
          | None -> modules
          | Some m ->
            (* These files needs to be alongside stdlib.cma as the
               compiler implicitly adds this module. *)
            [ Cm_kind.Cmx, (Cm_kind.ext Cmx)
            ; Cmo, (Cm_kind.ext Cmo)
            ; Cmx, ext_obj ]
            |> List.iter ~f:(fun (kind, ext) ->
              let src =
                Path.build (Obj_dir.Module.obj_file obj_dir m ~kind ~ext) in
              let dst = Path.Build.relative dir ((Module.obj_name m) ^ ext) in
              SC.add_rule sctx ~dir (Build.copy ~src ~dst));
            Module.Name.Map.remove modules name
        end
      | _ ->
        modules
    in
    let modules = List.rev_append
                    (Module.Name_map.impl_only modules)
                    (Vimpl.impl_only vimpl) in
    let wrapped_compat = Module.Name.Map.values wrapped_compat in
    (* Compatibility modules have implementations so we can just append them.
       We append the modules at the end as no library modules depend on
       them. *)
    let top_sorted_modules =
      Dep_graph.top_closed_implementations dep_graphs.impl modules
      (* TODO this is broken for vlibs that introduce wrapped compat *)
      >>^ fun modules -> modules @ wrapped_compat
    in

    let modes = Mode_conf.Set.eval lib.modes ~has_native in
    (let modules = modules @ wrapped_compat in
     Mode.Dict.Set.iter modes ~f:(fun mode ->
       build_lib lib ~obj_dir ~expander ~flags ~dir ~mode ~top_sorted_modules
         ~modules));
    (* Build *.cma.js *)
    if modes.byte then
      SC.add_rules sctx ~dir (
        let src =
          Library.archive lib ~dir ~ext:(Mode.compiled_lib_ext Mode.Byte) in
        let target =
          Path.Build.relative
            (Obj_dir.obj_dir obj_dir)
            (Path.Build.basename src)
          |> Path.Build.extend_basename ~suffix:".js" in
        Js_of_ocaml_rules.build_cm cctx ~js_of_ocaml ~src ~target);
    if Dynlink_supported.By_the_os.get natdynlink_supported
    && modes.native then
      build_shared lib ~dir ~flags ~ctx

  let library_rules (lib : Library.t) ~dir_contents ~dir ~expander ~scope
        ~compile_info ~dir_kind =
    let dep_kind =
      if lib.optional then Lib_deps_info.Kind.Optional else Required
    in
    let flags = SC.ocaml_flags sctx ~dir lib.buildable in
    let lib_modules =
      Dir_contents.modules_of_library dir_contents ~name:(Library.best_name lib)
    in
    let obj_dir = Library.obj_dir ~dir lib in
    Check_rules.add_obj_dir sctx ~obj_dir;
    let source_modules = Lib_modules.modules lib_modules in
    let vimpl = Virtual_rules.impl sctx ~lib ~dir ~scope in
    Option.iter vimpl ~f:(Virtual_rules.setup_copy_rules_for_impl ~sctx ~dir);
    (* Preprocess before adding the alias module as it doesn't need
       preprocessing *)
    let pp =
      Preprocessing.make sctx ~dir ~dep_kind ~scope
        ~preprocess:lib.buildable.preprocess
        ~expander
        ~preprocessor_deps:
          (SC.Deps.interpret sctx ~expander lib.buildable.preprocessor_deps)
        ~lint:lib.buildable.lint
        ~lib_name:(Some (snd lib.name))
        ~dir_kind
    in

    let lib_modules =
      Preprocessing.pp_modules pp source_modules
      |> Lib_modules.set_modules lib_modules
    in

    let alias_module = Lib_modules.alias_module lib_modules in
    let for_compilation = Lib_modules.for_compilation lib_modules in

    let cctx =
      let requires_compile = Lib.Compile.direct_requires compile_info in
      let requires_link    = Lib.Compile.requires_link compile_info in
      let dynlink =
        Dynlink_supported.get lib.dynlink ctx.supports_shared_libraries in
      Compilation_context.create ()
        ~super_context:sctx
        ~expander
        ?vimpl
        ~scope
        ~dir_kind
        ~obj_dir
        ~modules:for_compilation
        ?alias_module
        ?lib_interface_module:(Lib_modules.lib_interface_module lib_modules)
        ~flags
        ~requires_compile
        ~requires_link
        ~preprocessing:pp
        ~no_keep_locs:lib.no_keep_locs
        ~opaque
        ~js_of_ocaml:lib.buildable.js_of_ocaml
        ~dynlink
        ?stdlib:lib.stdlib
        ~package:(Option.map lib.public ~f:(fun p -> p.package))
    in

    let requires_compile = Compilation_context.requires_compile cctx in

    build_wrapped_compat_modules lib cctx ~lib_modules;

    let dep_graphs =
      let dep_graphs = Ocamldep.rules cctx in
      match vimpl with
      | None -> dep_graphs
      | Some impl ->
        let vlib = Vimpl.vlib_dep_graph impl in
        Dep_graph.Ml_kind.merge_for_impl ~vlib ~impl:dep_graphs
    in

    Lib_modules.modules lib_modules
    |> Module.Name.Map.iter
         ~f:(Module_compilation.build_module cctx ~dep_graphs);

    if Option.is_none lib.stdlib then begin
      Lib_modules.alias_module lib_modules
      |> Option.iter ~f:(fun alias_module ->
        let loc = lib.buildable.loc in
        build_alias_module ~loc ~alias_module ~dir ~lib_modules ~cctx)
    end;

    let expander = Super_context.expander sctx ~dir in

    let vlib_stubs_o_files = Vimpl.vlib_stubs_o_files vimpl in
    if Library.has_stubs lib || not (List.is_empty vlib_stubs_o_files) then
      build_stubs lib ~dir ~expander ~requires:requires_compile
        ~dir_contents ~vlib_stubs_o_files;

    if not (Library.is_virtual lib) then (
      let wrapped_compat = Lib_modules.wrapped_compat lib_modules in
      setup_build_archives lib ~wrapped_compat ~cctx ~dep_graphs ~expander);

    Odoc.setup_library_odoc_rules sctx lib ~obj_dir ~requires:requires_compile
      ~modules:for_compilation ~dep_graphs ~scope;

    let flags =
      match alias_module with
      | None -> Ocaml_flags.common flags
      | Some m ->
        Ocaml_flags.prepend_common
          ["-open"; Module.Name.to_string (Module.name m)] flags
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
     let objs_dirs =
       Obj_dir.of_local obj_dir
       |> Obj_dir.all_cmis
       |> Path.Set.of_list
     in
     Merlin.make ()
       ~requires:requires_compile
       ~flags
       ~preprocess:(Buildable.single_preprocess lib.buildable)
       ~libname:(snd lib.name)
       ~objs_dirs
    )

  let rules (lib : Library.t) ~dir_contents ~dir ~expander ~scope
        ~dir_kind : Compilation_context.t * Merlin.t =
    let compile_info =
      Lib.DB.get_compile_info (Scope.libs scope) (Library.best_name lib)
        ~allow_overlaps:lib.buildable.allow_overlapping_dependencies
    in
    let f () =
      library_rules lib ~dir_contents ~dir ~scope ~expander ~compile_info
        ~dir_kind
    in
    SC.Libs.gen_select_rules sctx compile_info ~dir;
    SC.Libs.with_lib_deps sctx compile_info ~dir ~f
end
