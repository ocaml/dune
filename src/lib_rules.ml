open! Stdune
open Import
open Build.O
open! No_io

module Buildable = Dune_file.Buildable
module Library = Dune_file.Library
module Mode_conf = Dune_file.Mode_conf

let msvc_hack_cclibs =
  List.map ~f:(fun lib ->
    let lib =
      match String.drop_prefix lib ~prefix:"-l" with
      | None -> lib
      | Some l -> l ^ ".lib"
    in
    Option.value ~default:lib (String.drop_prefix ~prefix:"-l" lib))

let build_lib (lib : Library.t) ~sctx ~expander ~flags ~dir ~mode ~cm_files =
  let ctx = Super_context.context sctx in
  let { Lib_config. ext_lib; _ } = ctx.lib_config in
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
    let obj_deps =
      Build.paths (Cm_files.unsorted_objects_and_cms cm_files ~mode) in
    let ocaml_flags = Ocaml_flags.get flags mode in
    let cclibs = Expander.expand_and_eval_set expander lib.c_library_flags
                   ~standard:(Build.return []) in
    let library_flags =
      Expander.expand_and_eval_set expander lib.library_flags
        ~standard:(Build.return []) in
    Super_context.add_rule ~dir sctx ~loc:lib.buildable.loc
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

let gen_wrapped_compat_modules (lib : Library.t) cctx =
  let modules = Compilation_context.modules cctx in
  let wrapped_compat = Modules.wrapped_compat modules in
  let transition_message = lazy (
    match Modules.wrapped modules with
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
    let sctx = Compilation_context.super_context cctx in
    Build.return contents
    >>> Build.write_file_dyn (Path.as_in_build_dir_exn source_path)
    |> Super_context.add_rule sctx ~loc ~dir:(Compilation_context.dir cctx)
  )

let build_c_file (lib : Library.t) ~sctx ~dir ~expander ~includes (loc, src, dst) =
  let ctx = Super_context.context sctx in
  let c_flags =
    (Super_context.c_flags sctx ~dir ~expander ~flags:lib.c_flags).c in
  Super_context.add_rule sctx ~loc ~dir
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
      (lib : Library.t) ~sctx ~dir ~expander ~includes (loc, src, dst) =
    let ctx = Super_context.context sctx in
  let output_param =
    if ctx.ccomp_type = "msvc" then
      [Command.Args.Concat ("", [A "/Fo"; Target dst])]
    else
      [A "-o"; Target dst]
  in
  let cxx_flags = (Super_context.c_flags sctx ~dir ~expander ~flags:lib.c_flags).cxx in
  Super_context.add_rule sctx ~loc ~dir (
    let src = Path.build (C.Source.path src) in
    Command.run
      (* We have to execute the rule in the library directory as
         the .o is produced in the current directory *)
      ~dir:(Path.build dir)
      (Super_context.resolve_program ~loc:None ~dir sctx ctx.c_compiler)
      ([ Command.Args.S [A "-I"; Path ctx.stdlib_dir]
       ; includes
       ; Command.Args.dyn cxx_flags
       ] @ output_param @
       [ A "-c"; Dep src
       ]));
  dst

let ocamlmklib (lib : Library.t) ~sctx ~dir ~expander ~o_files ~sandbox ~custom
      ~targets =
  Super_context.add_rule sctx ~sandbox ~dir
    ~loc:lib.buildable.loc
    (let cclibs_args =
       Expander.expand_and_eval_set expander
         lib.c_library_flags ~standard:(Build.return []) in
     let ctx = Super_context.context sctx in
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

let build_self_stubs lib ~sctx ~expander ~dir ~o_files =
  let ctx = Super_context.context sctx in
  let { Lib_config. ext_lib; ext_dll; has_native; _ } = ctx.lib_config in
  let static = Library.stubs_archive lib ~dir ~ext_lib in
  let dynamic = Library.dll lib ~dir ~ext_dll in
  let modes = Mode_conf.Set.eval lib.modes ~has_native in
  let ocamlmklib = ocamlmklib lib ~sctx ~expander ~dir ~o_files in
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

let build_o_files lib ~sctx ~(c_sources : C.Sources.t)
      ~dir ~expander ~requires ~dir_contents =
  let ctx = Super_context.context sctx in
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
                       S [ Lib.L.c_include_flags libs
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
      build_x lib ~sctx ~dir ~expander ~includes (loc, src, dst)
    )
  in
  let { C.Kind.Dict. c; cxx } = C.Sources.split_by_kind c_sources in
  build_x_files build_c_file c
  @ build_x_files build_cxx_file cxx
  |> List.map ~f:Path.build

let build_stubs lib ~sctx ~dir ~expander ~requires ~dir_contents
      ~vlib_stubs_o_files =
  let lib_o_files =
    if Library.has_stubs lib then
      let c_sources =
        Dir_contents.c_sources_of_library
          dir_contents ~name:(Library.best_name lib) in
      build_o_files lib ~sctx ~dir ~expander ~requires ~dir_contents ~c_sources
    else
      []
  in
  match vlib_stubs_o_files @ lib_o_files with
  | [] -> ()
  | o_files -> build_self_stubs lib ~sctx ~dir ~expander ~o_files

let build_shared lib ~sctx ~dir ~flags =
  let ctx = Super_context.context sctx in
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
    Super_context.add_rule sctx build ~dir)

let setup_build_archives (lib : Dune_file.Library.t)
      ~cctx ~(dep_graphs : Dep_graph.Ml_kind.t) ~expander =
  let dir = Compilation_context.dir cctx in
  let obj_dir = Compilation_context.obj_dir cctx in
  let flags = Compilation_context.flags cctx in
  let modules = Compilation_context.modules cctx in
  let js_of_ocaml = lib.buildable.js_of_ocaml in
  let sctx = Compilation_context.super_context cctx in
  let ctx = Compilation_context.context cctx in
  let { Lib_config. ext_obj; has_native; natdynlink_supported; _ } =
    ctx.lib_config in
  let impl_only = Modules.impl_only modules in
  Modules.exit_module modules
  |> Option.iter ~f:(fun m ->
    (* These files needs to be alongside stdlib.cma as the
       compiler implicitly adds this module. *)
    [ Cm_kind.Cmx, (Cm_kind.ext Cmx)
    ; Cmo, (Cm_kind.ext Cmo)
    ; Cmx, ext_obj ]
    |> List.iter ~f:(fun (kind, ext) ->
      let src =
        Path.build (Obj_dir.Module.obj_file obj_dir m ~kind ~ext) in
      let dst = Path.Build.relative dir ((Module.obj_name m) ^ ext) in
      Super_context.add_rule sctx ~dir (Build.copy ~src ~dst)));
  let top_sorted_modules =
    Dep_graph.top_closed_implementations dep_graphs.impl impl_only in

  let modes = Mode_conf.Set.eval lib.modes ~has_native in
  (let cm_files =
     Cm_files.make ~obj_dir ~ext_obj ~modules ~top_sorted_modules in
   Mode.Dict.Set.iter modes ~f:(fun mode ->
     build_lib lib ~sctx ~expander ~flags ~dir ~mode ~cm_files));
  (* Build *.cma.js *)
  if modes.byte then
    Super_context.add_rules sctx ~dir (
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
    build_shared ~sctx lib ~dir ~flags

let library_rules (lib : Library.t) ~sctx ~dir_contents ~dir ~expander ~scope
      ~compile_info ~dir_kind =
  let dep_kind =
    if lib.optional then Lib_deps_info.Kind.Optional else Required
  in
  let flags = Super_context.ocaml_flags sctx ~dir lib.buildable in
  let modules =
    Dir_contents.modules_of_library dir_contents ~name:(Library.best_name lib)
  in
  let obj_dir = Library.obj_dir ~dir lib in
  Check_rules.add_obj_dir sctx ~obj_dir;
  let vimpl = Virtual_rules.impl sctx ~lib ~scope in
  Option.iter vimpl ~f:(Virtual_rules.setup_copy_rules_for_impl ~sctx ~dir);
  (* Preprocess before adding the alias module as it doesn't need
     preprocessing *)
  let pp =
    Preprocessing.make sctx ~dir ~dep_kind ~scope
      ~preprocess:lib.buildable.preprocess
      ~expander
      ~preprocessor_deps:
        (Super_context.Deps.interpret sctx ~expander lib.buildable.preprocessor_deps)
      ~lint:lib.buildable.lint
      ~lib_name:(Some (snd lib.name))
      ~dir_kind
  in

  let source_modules =
    Modules.fold_user_written modules ~init:[] ~f:(fun m acc -> m :: acc)
  in
  let modules =
    Modules.map_user_written modules ~f:(Preprocessing.pp_module pp)
  in
  let modules = Vimpl.impl_modules vimpl modules in

  let cctx =
    let requires_compile = Lib.Compile.direct_requires compile_info in
    let requires_link    = Lib.Compile.requires_link compile_info in
    let ctx = Super_context.context sctx in
    let opaque = Super_context.opaque sctx in
    let dynlink =
      Dynlink_supported.get lib.dynlink ctx.supports_shared_libraries in
    Compilation_context.create ()
      ~super_context:sctx
      ~expander
      ~scope
      ~dir_kind
      ~obj_dir
      ~modules
      ~flags
      ~requires_compile
      ~requires_link
      ~preprocessing:pp
      ~no_keep_locs:lib.no_keep_locs
      ~opaque
      ~js_of_ocaml:(Some lib.buildable.js_of_ocaml)
      ~dynlink
      ?stdlib:lib.stdlib
      ~package:(Option.map lib.public ~f:(fun p -> p.package))
      ?vimpl
  in

  let requires_compile = Compilation_context.requires_compile cctx in
  let dep_graphs = Dep_rules.rules cctx ~modules in

  gen_wrapped_compat_modules lib cctx;
  Module_compilation.build_all cctx ~dep_graphs;

  let expander = Super_context.expander sctx ~dir in

  let vlib_stubs_o_files = Vimpl.vlib_stubs_o_files vimpl in
  if Library.has_stubs lib || not (List.is_empty vlib_stubs_o_files) then
    build_stubs lib ~sctx ~dir ~expander ~requires:requires_compile
      ~dir_contents ~vlib_stubs_o_files;

  if not (Library.is_virtual lib) then (
    setup_build_archives lib ~cctx ~dep_graphs ~expander);

  Odoc.setup_library_odoc_rules cctx lib ~dep_graphs;

  Sub_system.gen_rules
    { super_context = sctx
    ; dir
    ; stanza = lib
    ; scope
    ; source_modules
    ; compile_info
    };

  ( cctx
  , Merlin.make ()
      ~requires:requires_compile
      ~flags
      ~modules
      ~preprocess:(Buildable.single_preprocess lib.buildable)
      ~libname:(snd lib.name)
      ~obj_dir
  )

let rules (lib : Library.t) ~sctx ~dir_contents ~dir ~expander ~scope
      ~dir_kind : Compilation_context.t * Merlin.t =
  let compile_info =
    Lib.DB.get_compile_info (Scope.libs scope) (Library.best_name lib)
      ~allow_overlaps:lib.buildable.allow_overlapping_dependencies
  in
  let f () =
    library_rules lib ~sctx ~dir_contents ~dir ~scope ~expander ~compile_info
      ~dir_kind
  in
  Super_context.Libs.gen_select_rules sctx compile_info ~dir;
  Super_context.Libs.with_lib_deps sctx compile_info ~dir ~f
