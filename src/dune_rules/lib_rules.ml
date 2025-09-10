open Import
open Memo.O

let cclibs =
  (* https://github.com/ocaml/dune/issues/119 *)
  let msvc_hack_cclibs =
    List.map ~f:(fun lib ->
      let lib =
        match String.drop_prefix lib ~prefix:"-l" with
        | None -> lib
        | Some l -> l ^ ".lib"
      in
      Option.value ~default:lib (String.drop_prefix ~prefix:"-l" lib))
  in
  fun (ccomp : Ocaml_config.Ccomp_type.t) ~flag c_library_flags ->
    Command.quote_args
      flag
      ((match ccomp with
        | Msvc -> msvc_hack_cclibs
        | Cc | Other _ -> Fun.id)
         c_library_flags)
;;

let standard_cxx_flags ~dir ~has_cxx sctx =
  let open Action_builder.O in
  let* project = Action_builder.of_memo (Dune_load.find_project ~dir) in
  match Dune_project.use_standard_c_and_cxx_flags project with
  | Some true when has_cxx () ->
    let ctx = Super_context.context sctx in
    Cc_flags.get_flags ~for_:Link (Context.build_context ctx)
  | _ -> Action_builder.return []
;;

let lib_args (mode : Mode.t) ~stub_mode archive =
  let lname = "-l" ^ Foreign.Archive.(name ~mode:stub_mode archive |> Name.to_string) in
  (match mode with
   | Native -> []
   | Byte -> [ "-dllib"; lname ])
  @ [ "-cclib"; lname ]
;;

(* Build an OCaml library. *)
let build_lib
      (lib : Library.t)
      ~native_archives
      ~sctx
      ~expander
      ~flags
      ~dir
      ~(mode : Mode.t)
      ~cm_files
  =
  let ctx = Super_context.context sctx in
  let* ocaml = Context.ocaml ctx in
  let map_cclibs = cclibs ocaml.lib_config.ccomp_type ~flag:"-cclib" in
  Ocaml_toolchain.compiler ocaml mode
  |> Memo.Result.iter ~f:(fun compiler ->
    [ Command.Args.dyn (Ocaml_flags.get flags (Ocaml mode))
    ; Hidden_deps (Cm_files.unsorted_objects_and_cms cm_files ~mode |> Dep.Set.of_files)
    ; A "-a"
    ; A "-o"
    ; Target (Library.archive lib ~dir ~ext:(Mode.compiled_lib_ext mode))
    ; As
        (let foreign_archives =
           Library.foreign_archives lib
           |> List.concat_map ~f:(lib_args mode ~stub_mode:All)
         in
         let stubs_archive =
           match Library.stubs_archive lib with
           | None -> []
           | Some lib_archive ->
             let stub_mode : Mode.Select.t =
               if Buildable.has_mode_dependent_foreign_stubs lib.buildable
               then Only mode
               else All
             in
             lib_args mode ~stub_mode lib_archive
         in
         stubs_archive @ foreign_archives)
    ; Dyn
        (let open Action_builder.O in
         let standard =
           standard_cxx_flags
             ~dir
             ~has_cxx:(fun () -> Buildable.has_foreign_cxx lib.buildable)
             sctx
         in
         Expander.expand_and_eval_set expander lib.c_library_flags ~standard
         >>| map_cclibs)
    ; Command.Args.dyn
        (let standard = Action_builder.return [] in
         Expander.expand_and_eval_set expander lib.library_flags ~standard)
    ; As
        (match lib.kind with
         | Virtual | Parameter | Dune_file Normal -> []
         | Dune_file (Ppx_deriver _ | Ppx_rewriter _) -> [ "-linkall" ])
    ; Dyn
        (Cm_files.top_sorted_cms cm_files ~mode
         |> Action_builder.map ~f:(fun x -> Command.Args.Deps x))
    ; Hidden_targets
        (match mode with
         | Byte -> []
         | Native -> native_archives)
    ; Dyn
        (let open Action_builder.O in
         Ctypes_rules.ctypes_cclib_flags sctx ~expander ~buildable:lib.buildable
         >>| map_cclibs)
    ; Deps
        (Foreign.Objects.build_paths
           lib.buildable.extra_objects
           ~ext_obj:ocaml.lib_config.ext_obj
           ~dir)
    ]
    |> Command.run (Ok compiler) ~dir:(Path.build (Context.build_dir ctx))
    |> Super_context.add_rule ~dir sctx ~loc:lib.buildable.loc)
;;

let gen_wrapped_compat_modules (lib : Library.t) cctx =
  let modules = Compilation_context.modules cctx in
  let transition_message =
    lazy
      (match Modules.With_vlib.wrapped modules with
       | Simple _ -> assert false
       | Yes_with_transition r -> r)
  in
  let main_module_name =
    lazy
      (match Library.main_module_name lib with
       | This (Some mmn) -> Module_name.to_string mmn
       | _ -> assert false)
  in
  Modules.With_vlib.wrapped_compat modules
  |> Module_name.Map.to_seq
  |> Memo.parallel_iter_seq ~f:(fun (name, m) ->
    let contents =
      let main_module_name = Lazy.force main_module_name in
      let open Action_builder.O in
      let+ () = Action_builder.return () in
      let name = Module_name.to_string name in
      let hidden_name = sprintf "%s__%s" main_module_name name in
      let real_name = sprintf "%s.%s" main_module_name name in
      sprintf
        {|[@@@deprecated "%s. Use %s instead."] include %s|}
        (Lazy.force transition_message)
        real_name
        hidden_name
    in
    let source_path = Option.value_exn (Module.file m ~ml_kind:Impl) in
    let loc = lib.buildable.loc in
    let sctx = Compilation_context.super_context cctx in
    Action_builder.write_file_dyn (Path.as_in_build_dir_exn source_path) contents
    |> Super_context.add_rule sctx ~loc ~dir:(Compilation_context.dir cctx))
;;

(* Rules for building static and dynamic libraries using [ocamlmklib]. *)
let ocamlmklib
      ~loc
      ~c_library_flags
      ~sctx
      ~dir
      ~o_files
      ~archive_name
      ~stubs_mode
      ~build_targets_together
  =
  let ctx = Super_context.context sctx in
  let* ocaml = Context.ocaml ctx in
  let build =
    let cclibs =
      Action_builder.map
        c_library_flags
        ~f:(cclibs ocaml.lib_config.ccomp_type ~flag:"-ldopt")
    in
    fun ~custom ~sandbox targets ->
      let open Action_builder.With_targets.O in
      let ctx = Super_context.context sctx in
      [ Command.Args.A "-g"
      ; (if custom then A "-custom" else Command.Args.empty)
      ; A "-o"
      ; Path (Path.build (Foreign.Archive.Name.path ~dir archive_name ~mode:stubs_mode))
      ; Command.Args.Dyn
          (Action_builder.map o_files ~f:(fun o_files -> Command.Args.Deps o_files))
        (* The [c_library_flags] is needed only for the [dynamic_target] case,
                but we pass them unconditionally for simplicity. *)
      ; Dyn cclibs
      ; Hidden_targets targets
      ]
      |> Command.run ~dir:(Path.build (Context.build_dir ctx)) ocaml.ocamlmklib
      >>| Action.Full.add_sandbox sandbox
      |> Super_context.add_rule sctx ~dir ~loc
  in
  let { Lib_config.ext_lib; ext_dll; _ } = ocaml.lib_config in
  let dynamic_target =
    Foreign.Archive.Name.dll_file archive_name ~dir ~ext_dll ~mode:stubs_mode
  in
  let static_target =
    Foreign.Archive.Name.lib_file archive_name ~dir ~ext_lib ~mode:stubs_mode
  in
  if build_targets_together
  then
    (* Build both the static and dynamic targets in one [ocamlmklib] invocation,
       unless dynamically linked foreign archives are disabled. *)
    Context.dynamically_linked_foreign_archives ctx
    >>| (function
     | true -> [ static_target; dynamic_target ]
     | false -> [ static_target ])
    >>= build ~sandbox:Sandbox_config.no_special_requirements ~custom:false
  else
    (* Build the static target only by passing the [-custom] flag. *)
    let* () =
      build ~sandbox:Sandbox_config.no_special_requirements ~custom:true [ static_target ]
    in
    (* The second rule (below) may fail on some platforms, but the build will
       succeed as long as the resulting dynamic library isn't actually needed
       (the rule will not fire in that case). We can't tell ocamlmklib to build
       only the dynamic target, so it will actually build *both* and we
       therefore sandbox the action to avoid overwriting the static archive.

       TODO: Figure out how to avoid duplicating work in the case when both
       rules fire. It seems like this might require introducing the notion of
       "optional targets", allowing us to run [ocamlmklib] with the [-failsafe]
       flag, which always produces the static target and sometimes produces the
       dynamic target too. *)
    let* dynamically_linked_foreign_archives =
      Context.dynamically_linked_foreign_archives ctx
    in
    Memo.when_ dynamically_linked_foreign_archives (fun () ->
      build ~sandbox:Sandbox_config.needs_sandboxing ~custom:false [ dynamic_target ])
;;

(* Build a static and a dynamic archive for a foreign library. Note that the
   dynamic archive can't be built on some platforms, in which case the rule that
   produces it will fail. *)
let foreign_rules (library : Foreign_library.t) ~sctx ~expander ~dir ~dir_contents =
  let archive_name = library.archive_name in
  let* foreign_sources =
    Dir_contents.foreign_sources dir_contents
    >>| Foreign_sources.for_archive ~archive_name
  in
  let* o_files =
    let* extra_o_files =
      let+ { Lib_config.ext_obj; _ } =
        let+ ocaml = Super_context.context sctx |> Context.ocaml in
        ocaml.lib_config
      in
      let open Action_builder.O in
      Expander.expand_and_eval_set
        expander
        library.extra_objects
        ~standard:(Action_builder.return [])
      >>| List.map ~f:(fun obj -> Path.build @@ Path.Build.relative dir (obj ^ ext_obj))
    in
    let+ o_files =
      Foreign_rules.build_o_files
        ~sctx
        ~dir
        ~expander
        ~requires:(Resolve.return [])
        ~dir_contents
        ~foreign_sources
    in
    let open Action_builder.O in
    extra_o_files >>| Mode.Map.Multi.add_all o_files All >>| Mode.Map.Multi.for_all_modes
  in
  let* () = Check_rules.add_files sctx ~dir o_files in
  let c_library_flags =
    let standard =
      standard_cxx_flags
        ~dir
        ~has_cxx:(fun () -> Foreign.Sources.has_cxx_sources foreign_sources)
        sctx
    in
    Expander.expand_and_eval_set expander Ordered_set_lang.Unexpanded.standard ~standard
  in
  ocamlmklib
    ~archive_name
    ~loc:library.stubs.loc
    ~c_library_flags
    ~sctx
    ~dir
    ~o_files
    ~build_targets_together:false
    ~stubs_mode:Mode.Select.All
;;

(* Build a required set of archives for an OCaml library. *)
let build_stubs lib ~cctx ~dir ~expander ~requires ~dir_contents ~vlib_stubs_o_files =
  let sctx = Compilation_context.super_context cctx in
  let* foreign_sources =
    let+ foreign_sources = Dir_contents.foreign_sources dir_contents in
    let name = Library.best_name lib in
    Foreign_sources.for_lib foreign_sources ~name
  in
  let* o_files =
    Foreign_rules.build_o_files
      ~sctx
      ~dir
      ~expander
      ~requires
      ~dir_contents
      ~foreign_sources
  in
  let all_o_files = Mode.Map.Multi.to_flat_list o_files in
  let* () = Check_rules.add_files sctx ~dir (Action_builder.return all_o_files) in
  if List.for_all ~f:List.is_empty [ all_o_files; vlib_stubs_o_files ]
  then Memo.return ()
  else (
    let modes = (Compilation_context.modes cctx).ocaml in
    let ocamlmklib =
      let build_targets_together =
        modes.native
        && modes.byte
        && Dynlink_supported.get_ocaml_config
             lib.dynlink
             (Compilation_context.ocaml cctx).ocaml_config
      in
      let archive_name =
        let lib_name = Lib_name.Local.to_string (snd lib.name) in
        Foreign.Archive.Name.stubs lib_name
      in
      let c_library_flags =
        let open Action_builder.O in
        let standard =
          standard_cxx_flags ~dir sctx ~has_cxx:(fun () ->
            Foreign.Sources.has_cxx_sources foreign_sources)
        in
        let+ c_lib = Expander.expand_and_eval_set expander lib.c_library_flags ~standard
        and+ ctypes_lib =
          (* CR rgrinberg: Should we add these flags to :standard? to make
           it possible for users to remove these *)
          Ctypes_rules.ctypes_cclib_flags sctx ~expander ~buildable:lib.buildable
        in
        c_lib @ ctypes_lib
      in
      ocamlmklib
        ~archive_name
        ~loc:lib.buildable.loc
        ~sctx
        ~dir
        ~c_library_flags
        ~build_targets_together
    in
    let for_all_modes =
      let lib_o_files_for_all_modes = Mode.Map.Multi.for_all_modes o_files in
      List.rev_append vlib_stubs_o_files lib_o_files_for_all_modes
    in
    if
      Mode.Dict.Set.to_list modes
      |> List.for_all ~f:(fun mode ->
        List.is_empty @@ Mode.Map.Multi.for_only ~and_all:false o_files mode)
    then
      (* if stubs are not mode dependent *)
      ocamlmklib ~o_files:(Action_builder.return for_all_modes) ~stubs_mode:All
    else
      Mode.Dict.Set.to_list modes
      |> List.map ~f:(fun mode ->
        let o_files_for_mode = Mode.Map.Multi.for_only ~and_all:false o_files mode in
        List.rev_append for_all_modes o_files_for_mode, Mode.Select.Only mode)
      |> Memo.parallel_iter ~f:(fun (o_files, stubs_mode) ->
        ocamlmklib ~o_files:(Action_builder.return o_files) ~stubs_mode))
;;

let build_shared (lib : Library.t) ~native_archives ~sctx ~dir ~flags =
  let ctx = Super_context.context sctx in
  let* ocaml = Context.ocaml ctx in
  Memo.Result.iter ocaml.ocamlopt ~f:(fun ocamlopt ->
    [ Command.Args.dyn (Ocaml_flags.get flags (Ocaml Native))
    ; Hidden_deps
        (let ext_lib = ocaml.lib_config.ext_lib in
         List.rev_concat
           [ Library.foreign_lib_files lib ~dir ~ext_lib ~for_mode:(Only Byte)
           ; Library.foreign_lib_files lib ~dir ~ext_lib ~for_mode:All
           ; native_archives
           ]
         |> List.rev_map ~f:Path.build
         |> Dep.Set.of_files)
    ; A "-shared"
    ; A "-linkall"
    ; A "-I"
    ; Path (Path.build dir)
    ; (let include_flags_for_relative_foreign_archives =
         List.map lib.buildable.foreign_archives ~f:(fun (_loc, archive) ->
           let dir = Foreign.Archive.dir_path ~dir archive in
           Command.Args.S [ A "-I"; Path (Path.build dir) ])
       in
       Command.Args.S include_flags_for_relative_foreign_archives)
    ; A "-o"
    ; Target
        (let ext = Mode.plugin_ext Native in
         Library.archive lib ~dir ~ext)
    ; Dep
        (let ext = Mode.compiled_lib_ext Native in
         Path.build (Library.archive lib ~dir ~ext))
    ]
    |> Command.run ~dir:(Path.build (Context.build_dir ctx)) (Ok ocamlopt)
    |> Super_context.add_rule sctx ~dir ~loc:lib.buildable.loc)
;;

let iter_modes_concurrently (t : _ Ocaml.Mode.Dict.t) ~(f : Ocaml.Mode.t -> unit Memo.t) =
  let+ () = Memo.when_ t.byte (fun () -> f Byte)
  and+ () = Memo.when_ t.native (fun () -> f Native) in
  ()
;;

let setup_build_archives (lib : Library.t) ~top_sorted_modules ~cctx ~expander ~lib_info =
  let obj_dir = Compilation_context.obj_dir cctx in
  let flags = Compilation_context.flags cctx in
  let modules = Compilation_context.modules cctx in
  let sctx = Compilation_context.super_context cctx in
  let { Lib_config.ext_obj; natdynlink_supported; _ } =
    let ocaml = Compilation_context.ocaml cctx in
    ocaml.lib_config
  in
  let* () =
    Modules.With_vlib.exit_module modules
    |> Memo.Option.iter ~f:(fun m ->
      (* These files needs to be alongside stdlib.cma as the compiler
         implicitly adds this module. *)
      [ Cm_kind.Cmx, Cm_kind.ext Cmx; Cmo, Cm_kind.ext Cmo; Cmx, ext_obj ]
      |> Memo.parallel_iter ~f:(fun (kind, ext) ->
        let symlink =
          let src =
            Path.build (Obj_dir.Module.obj_file obj_dir m ~kind:(Ocaml kind) ~ext)
          in
          let dst =
            (* XXX we should get the directory from the dir of the cma
               file explicitly *)
            Module.obj_name m
            |> Module_name.Unique.artifact_filename ~ext
            |> Path.Build.relative (Obj_dir.dir obj_dir)
          in
          Action_builder.symlink ~src ~dst
        in
        let dir = Compilation_context.dir cctx in
        Super_context.add_rule sctx ~dir ~loc:lib.buildable.loc symlink))
  in
  let modes = Compilation_context.modes cctx in
  (* The [dir] below is used as an object directory without going through
     [Obj_dir]. That's fragile and will break if the layout of the object
     directory changes *)
  let dir = Obj_dir.dir obj_dir in
  let native_archives =
    Lib_info.eval_native_archives_exn lib_info ~modules:(Some modules)
  in
  let* () =
    match lib.kind with
    | Parameter -> Memo.return ()
    | Virtual | Dune_file _ ->
      let cm_files =
        let excluded_modules =
          (* ctypes type_gen and function_gen scripts should not be included in the
           library. Otherwise they will spew stuff to stdout on library load. *)
          match lib.buildable.ctypes with
          | Some ctypes -> Ctypes_field.non_installable_modules ctypes
          | None -> []
        in
        Cm_files.make ~excluded_modules ~obj_dir ~ext_obj ~modules ~top_sorted_modules ()
      in
      iter_modes_concurrently modes.ocaml ~f:(fun mode ->
        build_lib lib ~native_archives ~dir ~sctx ~expander ~flags ~mode ~cm_files)
  and* () =
    (* Build *.cma.js / *.wasma *)
    Memo.when_ modes.ocaml.byte (fun () ->
      let src = Library.archive lib ~dir ~ext:(Mode.compiled_lib_ext Mode.Byte) in
      Memo.parallel_iter Js_of_ocaml.Mode.all ~f:(fun mode ->
        let action_with_targets =
          List.map Jsoo_rules.Config.all ~f:(fun config ->
            Jsoo_rules.build_cm
              cctx
              ~dir
              ~in_context:
                (Js_of_ocaml.In_context.make ~dir lib.buildable.js_of_ocaml
                 |> Js_of_ocaml.Mode.Pair.select ~mode)
              ~mode
              ~config:(Some config)
              ~src:(Path.build src)
              ~deps:(Action_builder.return [])
              ~obj_dir)
        in
        Memo.parallel_iter action_with_targets ~f:(fun rule ->
          Super_context.add_rule sctx ~dir ~loc:lib.buildable.loc rule)))
  in
  Memo.when_
    (Lib_info.dynlink_supported lib_info
     && Dynlink_supported.By_the_os.get natdynlink_supported
     && modes.ocaml.native)
    (fun () -> build_shared ~native_archives ~sctx lib ~dir ~flags)
;;

let cctx (lib : Library.t) ~sctx ~source_modules ~dir ~expander ~scope ~compile_info =
  let* flags = Buildable_rules.ocaml_flags sctx ~dir lib.buildable.flags
  and* implements = Virtual_rules.impl sctx ~lib ~scope in
  let obj_dir = Library.obj_dir ~dir lib in
  let* modules, pp =
    Buildable_rules.modules_rules
      sctx
      (Library (lib.buildable, snd lib.name))
      expander
      ~dir
      scope
      source_modules
  in
  let modules = Virtual_rules.impl_modules implements modules in
  let requires_compile = Lib.Compile.direct_requires compile_info in
  let requires_link = Lib.Compile.requires_link compile_info in
  let* modes =
    let+ ocaml =
      let ctx = Super_context.context sctx in
      Context.ocaml ctx
    in
    let { Lib_config.has_native; _ } = ocaml.lib_config in
    Mode_conf.Lib.Set.eval_detailed lib.modes ~has_native
  in
  let package = Library.package lib in
  let js_of_ocaml = Js_of_ocaml.In_context.make ~dir lib.buildable.js_of_ocaml in
  (* XXX(anmonteiro): `melange_package_name` is used to derive Melange's
     `--mel-package-name` argument. We only use the library name for public
     libraries / private libraries with `(package ..)` because we need Melange
     to preserve relative paths for private libs (i.e. not pass the
     `--mel-package-name` arg). *)
  let melange_package_name =
    match lib.visibility with
    | Public p -> Some (Public_lib.name p)
    | Private (Some pkg) -> Some (Lib_name.mangled (Package.name pkg) (snd lib.name))
    | Private None -> None
  in
  Compilation_context.create
    ()
    ~super_context:sctx
    ~scope
    ~obj_dir
    ~modules
    ~flags
    ~requires_compile
    ~requires_link
    ~implements
    ~preprocessing:pp
    ~opaque:Inherit_from_settings
    ~js_of_ocaml:(Js_of_ocaml.Mode.Pair.map ~f:Option.some js_of_ocaml)
    ?stdlib:lib.stdlib
    ~package
    ~melange_package_name
    ~modes
;;

let library_rules
      (lib : Library.t)
      ~local_lib
      ~cctx
      ~source_modules
      ~dir_contents
      ~compile_info
      ~ctx_dir
  =
  let modules = Compilation_context.modules cctx in
  let obj_dir = Compilation_context.obj_dir cctx in
  let implements = Compilation_context.implements cctx in
  let sctx = Compilation_context.super_context cctx in
  let dir = Compilation_context.dir cctx in
  let scope = Compilation_context.scope cctx in
  let* requires_compile = Compilation_context.requires_compile cctx in
  let lib_config = (Compilation_context.ocaml cctx).lib_config in
  let top_sorted_modules =
    let impl_only = Modules.With_vlib.impl_only modules in
    Dep_graph.top_closed_implementations
      (Compilation_context.dep_graphs cctx).impl
      impl_only
  in
  let* () = Virtual_rules.setup_copy_rules_for_impl ~sctx ~dir implements in
  let* expander = Super_context.expander sctx ~dir in
  let* () = Check_rules.add_cycle_check sctx ~dir top_sorted_modules in
  let* () = gen_wrapped_compat_modules lib cctx
  and* () = Module_compilation.build_all cctx
  and* lib_info =
    let info =
      Library.to_lib_info
        lib
        ~expander:(Memo.return (Expander.to_expander0 expander))
        ~dir
        ~lib_config
    in
    let mode = Lib_mode.Map.Set.for_merlin (Lib_info.modes info) in
    let+ () = Check_rules.add_obj_dir sctx ~obj_dir mode in
    info
  in
  let+ () =
    Memo.when_ (Compilation_context.bin_annot cctx) (fun () ->
      Ocaml_index.cctx_rules cctx)
  and+ () =
    Memo.when_
      (not (Library.is_virtual lib))
      (fun () -> setup_build_archives lib ~lib_info ~top_sorted_modules ~cctx ~expander)
  and+ () =
    let vlib_stubs_o_files = Virtual_rules.stubs_o_files implements in
    Memo.when_
      (Library.has_foreign lib || List.is_non_empty vlib_stubs_o_files)
      (fun () ->
         build_stubs
           lib
           ~cctx
           ~dir
           ~expander
           ~requires:requires_compile
           ~dir_contents
           ~vlib_stubs_o_files)
  and+ () = Odoc.setup_private_library_doc_alias sctx ~scope ~dir:ctx_dir lib
  and+ () = Odoc.setup_library_odoc_rules cctx local_lib
  and+ () =
    let source_modules =
      Modules.fold_user_written source_modules ~init:[] ~f:(fun m acc -> m :: acc)
    in
    Sub_system.gen_rules
      { super_context = sctx; dir; stanza = lib; scope; source_modules; compile_info }
  and+ merlin =
    let+ requires_hidden = Compilation_context.requires_hidden cctx in
    let flags = Compilation_context.flags cctx in
    Merlin.make
      ~requires_compile
      ~requires_hidden
      ~stdlib_dir:lib_config.stdlib_dir
      ~flags
      ~modules
      ~preprocess:(Preprocess.Per_module.without_instrumentation lib.buildable.preprocess)
      ~libname:(Some (snd lib.name))
      ~obj_dir
      ~dialects:(Dune_project.dialects (Scope.project scope))
      ~ident:(Merlin_ident.for_lib (Library.best_name lib))
      ~modes:(`Lib (Lib_info.modes lib_info))
  in
  merlin
;;

let rules (lib : Library.t) ~sctx ~dir_contents ~expander ~scope =
  let dir = Dir_contents.dir dir_contents in
  let buildable = lib.buildable in
  let libs = Scope.libs scope in
  let lib_id =
    let src_dir = Path.Build.drop_build_context_exn dir in
    Library.to_lib_id ~src_dir lib
  in
  let* local_lib, compile_info =
    Lib.DB.get_compile_info
      libs
      (Local lib_id)
      ~allow_overlaps:buildable.allow_overlapping_dependencies
  in
  let f () =
    let* source_modules =
      Dir_contents.ocaml dir_contents >>= Ml_sources.modules ~libs ~for_:(Library lib_id)
    in
    let* cctx = cctx lib ~sctx ~source_modules ~dir ~scope ~expander ~compile_info in
    let* () =
      match buildable.ctypes with
      | None -> Memo.return ()
      | Some _ ->
        Ctypes_rules.gen_rules ~loc:(fst lib.name) ~cctx ~buildable ~sctx ~scope ~dir
    in
    let+ merlin =
      library_rules
        lib
        ~local_lib:(Lib.Local.of_lib_exn local_lib)
        ~cctx
        ~source_modules
        ~dir_contents
        ~compile_info
        ~ctx_dir:dir
    in
    cctx, merlin
  in
  let* () = Buildable_rules.gen_select_rules sctx compile_info ~dir in
  let merlin_ident = Merlin_ident.for_lib (Library.best_name lib) in
  Buildable_rules.with_lib_deps (Super_context.context sctx) merlin_ident ~dir ~f
;;
