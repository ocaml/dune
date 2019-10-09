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

(* Build an OCaml library. *)
let build_lib (lib : Library.t) ~sctx ~expander ~flags ~dir ~mode ~cm_files =
  let ctx = Super_context.context sctx in
  let { Lib_config.ext_lib; _ } = ctx.lib_config in
  Option.iter (Context.compiler ctx mode) ~f:(fun compiler ->
      let target =
        Library.archive lib ~dir ~ext:(Mode.compiled_lib_ext mode)
      in
      let stubs_flags =
        List.concat_map (Library.archive_names lib) ~f:(fun name ->
            let lname = "-l" ^ name in
            let cclib = [ "-cclib"; lname ] in
            let dllib = [ "-dllib"; lname ] in
            match mode with
            | Native -> cclib
            | Byte -> dllib @ cclib)
      in
      let map_cclibs =
        (* https://github.com/ocaml/dune/issues/119 *)
        if ctx.ccomp_type = "msvc" then
          msvc_hack_cclibs
        else
          Fn.id
      in
      let obj_deps =
        Build.paths (Cm_files.unsorted_objects_and_cms cm_files ~mode)
      in
      let ocaml_flags = Ocaml_flags.get flags mode in
      let cclibs =
        Expander.expand_and_eval_set expander lib.c_library_flags
          ~standard:(Build.return [])
      in
      let library_flags =
        Expander.expand_and_eval_set expander lib.library_flags
          ~standard:(Build.return [])
      in
      Super_context.add_rule ~dir sctx ~loc:lib.buildable.loc
        ( obj_deps
        >>> Command.run (Ok compiler) ~dir:(Path.build ctx.build_dir)
              [ Command.Args.dyn ocaml_flags
              ; A "-a"
              ; A "-o"
              ; Target target
              ; As stubs_flags
              ; Dyn
                  (Build.map cclibs ~f:(fun x ->
                       Command.quote_args "-cclib" (map_cclibs x)))
              ; Command.Args.dyn library_flags
              ; As
                  ( match lib.kind with
                  | Normal -> []
                  | Ppx_deriver _
                  | Ppx_rewriter _ ->
                    [ "-linkall" ] )
              ; Dyn
                  ( Cm_files.top_sorted_cms cm_files ~mode
                  |> Build.map ~f:(fun x -> Command.Args.Deps x) )
              ; Hidden_targets
                  ( match mode with
                  | Byte -> []
                  | Native -> [ Library.archive lib ~dir ~ext:ext_lib ] )
              ] ))

let gen_wrapped_compat_modules (lib : Library.t) cctx =
  let modules = Compilation_context.modules cctx in
  let wrapped_compat = Modules.wrapped_compat modules in
  let transition_message =
    lazy
      ( match Modules.wrapped modules with
      | Simple _ -> assert false
      | Yes_with_transition r -> r )
  in
  Module_name.Map.iteri wrapped_compat ~f:(fun name m ->
      let main_module_name =
        match Library.main_module_name lib with
        | This (Some mmn) -> Module_name.to_string mmn
        | _ -> assert false
      in
      let contents =
        let name = Module_name.to_string name in
        let hidden_name = sprintf "%s__%s" main_module_name name in
        let real_name = sprintf "%s.%s" main_module_name name in
        sprintf {|[@@@deprecated "%s. Use %s instead."] include %s|}
          (Lazy.force transition_message)
          real_name hidden_name
      in
      let source_path = Option.value_exn (Module.file m ~ml_kind:Impl) in
      let loc = lib.buildable.loc in
      let sctx = Compilation_context.super_context cctx in
      Build.write_file (Path.as_in_build_dir_exn source_path) contents
      |> Super_context.add_rule sctx ~loc ~dir:(Compilation_context.dir cctx))

(* Add a rule calling [ocamlmklib] to build a library. *)
let ocamlmklib ~path ~loc ~c_library_flags ~sctx ~dir ~expander ~o_files
    ~sandbox ~custom ~targets =
  Super_context.add_rule sctx ~sandbox ~dir ~loc
    (let cclibs_args =
       Expander.expand_and_eval_set expander c_library_flags
         ~standard:(Build.return [])
     in
     let ctx = Super_context.context sctx in
     Command.run ~dir:(Path.build ctx.build_dir) (Ok ctx.ocamlmklib)
       [ A "-g"
       ; ( if custom then
           A "-custom"
         else
           Command.Args.empty )
       ; A "-o"
       ; Path path
       ; Deps o_files
       ; Dyn
           (Build.map cclibs_args ~f:(fun cclibs ->
                (* https://github.com/ocaml/dune/issues/119 *)
                if ctx.ccomp_type = "msvc" then
                  let cclibs = msvc_hack_cclibs cclibs in
                  Command.quote_args "-ldopt" cclibs
                else
                  As cclibs))
       ; Hidden_targets targets
       ])

(* Add a rule calling [ocamlmklib] to build an OCaml library. *)
let ocamlmklib_ocaml (lib : Library.t) ~sctx ~dir ~expander ~o_files ~sandbox
    ~custom ~targets =
  let path =
    Path.build (Path.Build.relative dir (Library.stubs_archive_name lib))
  in
  ocamlmklib ~path ~loc:lib.buildable.loc ~c_library_flags:lib.c_library_flags
    ~sctx ~dir ~expander ~o_files ~sandbox ~custom ~targets

(* Build a static and a dynamic archive for a foreign library. *)
let build_foreign_library (library : Foreign.Library.t) ~sctx ~expander ~dir
    ~dir_contents =
  let archive_name = library.archive_name in
  let o_files =
    let include_dirs_loc, include_dirs = library.stubs.include_dirs in
    let include_dirs =
      Build.map
        ~f:(List.map ~f:(Path.relative (Path.build dir)))
        (Expander.expand_and_eval_set expander include_dirs
           ~standard:(Build.return []))
    in
    let extra_flags =
      Command.Args.Dyn
        (Build.dyn_path_set
           (Build.map include_dirs ~f:(fun include_dirs ->
                let args, deps =
                  List.unzip
                    (List.map include_dirs ~f:(fun include_dir ->
                         let args =
                           Command.Args.S [ A "-I"; Path include_dir ]
                         in
                         let deps =
                           match
                             Path.extract_build_context_dir include_dir
                           with
                           | None ->
                             (* TODO: Track files in external directories. *)
                             User_warning.emit ~loc:include_dirs_loc
                               [ Pp.textf
                                   "%S is an external directory; dependencies \
                                    in external directories are currently not \
                                    tracked."
                                   (Path.to_string include_dir)
                               ];
                             Path.Set.empty
                           | Some (build_dir, source_dir) -> (
                             match File_tree.find_dir source_dir with
                             | None ->
                               User_error.raise ~loc:include_dirs_loc
                                 [ Pp.textf "Include directory %S not found."
                                     (Path.reach ~from:(Path.build dir)
                                        include_dir)
                                 ]
                             | Some dir ->
                               File_tree.Dir.fold dir
                                 ~traverse:Sub_dirs.Status.Set.all
                                 ~init:Path.Set.empty ~f:(fun t ->
                                   let paths =
                                     Path.Source.Set.to_list
                                       (File_tree.Dir.file_paths t)
                                     |> List.map
                                          ~f:(Path.append_source build_dir)
                                   in
                                   Path.Set.union (Path.Set.of_list paths)) )
                         in
                         (args, deps)))
                in
                (Command.Args.S args, Path.Set.union_all deps))))
    in
    let foreign_sources =
      Dir_contents.foreign_sources_of_archive dir_contents ~archive_name
    in
    Foreign_rules.build_o_files ~sctx ~dir ~expander ~requires:(Result.ok [])
      ~dir_contents ~foreign_sources ~extra_flags
      ~extra_deps:library.stubs.extra_deps
    |> List.map ~f:Path.build
  in
  Check_rules.add_files sctx ~dir o_files;
  let ctx = Super_context.context sctx in
  let { Lib_config.ext_lib; ext_dll; _ } = ctx.lib_config in
  let static = Foreign.lib_file ~archive_name ~dir ~ext_lib in
  let dynamic = Foreign.dll_file ~archive_name ~dir ~ext_dll in
  ocamlmklib
    ~path:(Path.build (Path.Build.relative dir archive_name))
    ~loc:library.stubs.loc
    ~c_library_flags:Ordered_set_lang.Unexpanded.standard ~sctx ~dir ~expander
    ~o_files ~sandbox:Sandbox_config.no_special_requirements ~custom:false
    ~targets:[ static; dynamic ]

(* Build a required set of archives for an OCaml library. *)
let build_self_stubs lib ~cctx ~expander ~dir ~o_files =
  let sctx = Compilation_context.super_context cctx in
  let ctx = Super_context.context sctx in
  let { Lib_config.ext_lib; ext_dll; _ } = ctx.lib_config in
  let archive_name = Library.stubs_archive_name lib in
  let static = Foreign.lib_file ~archive_name ~dir ~ext_lib in
  let dynamic = Foreign.dll_file ~archive_name ~dir ~ext_dll in
  let modes = Compilation_context.modes cctx in
  let ocamlmklib = ocamlmklib_ocaml lib ~sctx ~expander ~dir ~o_files in
  if
    modes.native && modes.byte
    && Dynlink_supported.get lib.dynlink ctx.supports_shared_libraries
  then
    (* If we build for both modes and support dynlink, use a single invocation
       to build both the static and dynamic libraries *)
    ocamlmklib ~sandbox:Sandbox_config.no_special_requirements ~custom:false
      ~targets:[ static; dynamic ]
  else (
    ocamlmklib ~sandbox:Sandbox_config.no_special_requirements ~custom:true
      ~targets:[ static ];
    (* We can't tell ocamlmklib to build only the dll, so we sandbox the action
       to avoid overriding the static archive *)
    ocamlmklib ~sandbox:Sandbox_config.needs_sandboxing ~custom:false
      ~targets:[ dynamic ]
  )

let build_stubs lib ~cctx ~dir ~expander ~requires ~dir_contents
    ~vlib_stubs_o_files =
  let sctx = Compilation_context.super_context cctx in
  let lib_o_files =
    let foreign_sources =
      Dir_contents.foreign_sources_of_library dir_contents
        ~name:(Library.best_name lib)
    in
    Foreign_rules.build_o_files ~sctx ~dir ~expander ~requires ~dir_contents
      ~foreign_sources ~extra_flags:Command.Args.empty ~extra_deps:[]
    |> List.map ~f:Path.build
  in
  Check_rules.add_files sctx ~dir lib_o_files;
  match vlib_stubs_o_files @ lib_o_files with
  | [] -> ()
  | o_files -> build_self_stubs lib ~cctx ~dir ~expander ~o_files

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
        Build.path (Path.build (Library.archive lib ~dir ~ext:ext_lib))
        >>> Command.run ~dir:(Path.build ctx.build_dir) (Ok ocamlopt)
              [ Command.Args.dyn (Ocaml_flags.get flags Native)
              ; A "-shared"
              ; A "-linkall"
              ; A "-I"
              ; Path (Path.build dir)
              ; A "-o"
              ; Target dst
              ; Dep src
              ]
      in
      let build =
        List.fold_left ~init:build (Library.lib_files lib ~dir ~ext_lib)
          ~f:(fun build lib -> Build.path (Path.build lib) >>> build)
      in
      Super_context.add_rule sctx build ~dir)

let setup_build_archives (lib : Dune_file.Library.t) ~cctx
    ~(dep_graphs : Dep_graph.Ml_kind.t) ~expander =
  let dir = Compilation_context.dir cctx in
  let obj_dir = Compilation_context.obj_dir cctx in
  let flags = Compilation_context.flags cctx in
  let modules = Compilation_context.modules cctx in
  let js_of_ocaml = lib.buildable.js_of_ocaml in
  let sctx = Compilation_context.super_context cctx in
  let ctx = Compilation_context.context cctx in
  let { Lib_config.ext_obj; natdynlink_supported; _ } = ctx.lib_config in
  let impl_only = Modules.impl_only modules in
  Modules.exit_module modules
  |> Option.iter ~f:(fun m ->
         (* These files needs to be alongside stdlib.cma as the compiler
            implicitly adds this module. *)
         [ (Cm_kind.Cmx, Cm_kind.ext Cmx)
         ; (Cmo, Cm_kind.ext Cmo)
         ; (Cmx, ext_obj)
         ]
         |> List.iter ~f:(fun (kind, ext) ->
                let src =
                  Path.build (Obj_dir.Module.obj_file obj_dir m ~kind ~ext)
                in
                let dst = Path.Build.relative dir (Module.obj_name m ^ ext) in
                Super_context.add_rule sctx ~dir (Build.copy ~src ~dst)));
  let top_sorted_modules =
    Dep_graph.top_closed_implementations dep_graphs.impl impl_only
  in
  let modes = Compilation_context.modes cctx in
  (let cm_files =
     Cm_files.make ~obj_dir ~ext_obj ~modules ~top_sorted_modules
   in
   Mode.Dict.Set.iter modes ~f:(fun mode ->
       build_lib lib ~sctx ~expander ~flags ~dir ~mode ~cm_files));
  (* Build *.cma.js *)
  if modes.byte then
    Super_context.add_rules sctx ~dir
      (let src =
         Library.archive lib ~dir ~ext:(Mode.compiled_lib_ext Mode.Byte)
       in
       let target =
         Path.Build.relative (Obj_dir.obj_dir obj_dir)
           (Path.Build.basename src)
         |> Path.Build.extend_basename ~suffix:".js"
       in
       Js_of_ocaml_rules.build_cm cctx ~js_of_ocaml ~src ~target);
  if Dynlink_supported.By_the_os.get natdynlink_supported && modes.native then
    build_shared ~sctx lib ~dir ~flags

let cctx (lib : Library.t) ~sctx ~source_modules ~dir ~expander ~scope
    ~compile_info =
  let dep_kind =
    if lib.optional then
      Lib_deps_info.Kind.Optional
    else
      Required
  in
  let flags = Super_context.ocaml_flags sctx ~dir lib.buildable in
  let obj_dir = Library.obj_dir ~dir lib in
  let vimpl = Virtual_rules.impl sctx ~lib ~scope in
  (* Preprocess before adding the alias module as it doesn't need preprocessing *)
  let pp =
    Preprocessing.make sctx ~dir ~dep_kind ~scope
      ~preprocess:lib.buildable.preprocess ~expander
      ~preprocessor_deps:lib.buildable.preprocessor_deps
      ~lint:lib.buildable.lint
      ~lib_name:(Some (snd lib.name))
  in
  let modules =
    Modules.map_user_written source_modules ~f:(Preprocessing.pp_module pp)
  in
  let modules = Vimpl.impl_modules vimpl modules in
  let requires_compile = Lib.Compile.direct_requires compile_info in
  let requires_link = Lib.Compile.requires_link compile_info in
  let ctx = Super_context.context sctx in
  let opaque = Super_context.opaque sctx in
  let dynlink =
    Dynlink_supported.get lib.dynlink ctx.supports_shared_libraries
  in
  let modes =
    let { Lib_config.has_native; _ } = ctx.lib_config in
    Dune_file.Mode_conf.Set.eval lib.modes ~has_native
  in
  Compilation_context.create () ~super_context:sctx ~expander ~scope ~obj_dir
    ~modules ~flags ~requires_compile ~requires_link ~preprocessing:pp ~opaque
    ~js_of_ocaml:(Some lib.buildable.js_of_ocaml) ~dynlink ?stdlib:lib.stdlib
    ~package:(Option.map lib.public ~f:(fun p -> p.package))
    ?vimpl ~modes

let library_rules (lib : Library.t) ~cctx ~source_modules ~dir_contents
    ~compile_info =
  (* Preprocess before adding the alias module as it doesn't need preprocessing *)
  let source_modules =
    Modules.fold_user_written source_modules ~init:[] ~f:(fun m acc ->
        m :: acc)
  in
  let modules = Compilation_context.modules cctx in
  let obj_dir = Compilation_context.obj_dir cctx in
  let vimpl = Compilation_context.vimpl cctx in
  let flags = Compilation_context.flags cctx in
  let sctx = Compilation_context.super_context cctx in
  let dir = Compilation_context.dir cctx in
  let scope = Compilation_context.scope cctx in
  let requires_compile = Compilation_context.requires_compile cctx in
  let dep_graphs = Dep_rules.rules cctx ~modules in
  Option.iter vimpl ~f:(Virtual_rules.setup_copy_rules_for_impl ~sctx ~dir);
  Check_rules.add_obj_dir sctx ~obj_dir;
  gen_wrapped_compat_modules lib cctx;
  Module_compilation.build_all cctx ~dep_graphs;
  let expander = Super_context.expander sctx ~dir in
  if not (Library.is_virtual lib) then
    setup_build_archives lib ~cctx ~dep_graphs ~expander;
  let () =
    let vlib_stubs_o_files = Vimpl.vlib_stubs_o_files vimpl in
    if Library.has_stubs lib || List.is_non_empty vlib_stubs_o_files then
      build_stubs lib ~cctx ~dir ~expander ~requires:requires_compile
        ~dir_contents ~vlib_stubs_o_files
  in
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
  , Merlin.make () ~requires:requires_compile ~flags ~modules
      ~preprocess:(Buildable.single_preprocess lib.buildable)
      ~libname:(snd lib.name) ~obj_dir )

let rules (lib : Library.t) ~sctx ~dir_contents ~dir ~expander ~scope :
    Compilation_context.t * Merlin.t =
  let compile_info =
    Lib.DB.get_compile_info (Scope.libs scope) (Library.best_name lib)
      ~allow_overlaps:lib.buildable.allow_overlapping_dependencies
  in
  let f () =
    let source_modules =
      Dir_contents.modules_of_library dir_contents
        ~name:(Library.best_name lib)
    in
    let cctx =
      cctx lib ~sctx ~source_modules ~dir ~scope ~expander ~compile_info
    in
    library_rules lib ~cctx ~source_modules ~dir_contents ~compile_info
  in
  Super_context.Libs.gen_select_rules sctx compile_info ~dir;
  Super_context.Libs.with_lib_deps sctx compile_info ~dir ~f
