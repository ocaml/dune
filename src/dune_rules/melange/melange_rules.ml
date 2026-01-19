open Import
open Memo.O

module Output_kind = struct
  type t =
    | Private_library_or_emit of Path.Build.t
    | Public_library of
        { lib_dir : Path.t
        ; target_dir : Path.Build.t
        ; output_dir : Path.Local.t
        }

  let[@ocaml.warning "-32"] to_dyn t =
    match t with
    | Private_library_or_emit dir ->
      Dyn.variant "Private_library_or_emit" [ Path.Build.to_dyn dir ]
    | Public_library { lib_dir; output_dir; target_dir } ->
      Dyn.variant
        "Public_library"
        [ Dyn.record
            [ "lib_dir", Path.to_dyn lib_dir
            ; "output_dir", Path.Local.to_dyn output_dir
            ; "target_dir", Path.Build.to_dyn target_dir
            ]
        ]
  ;;
end

let output_of_lib =
  let public_lib ~info ~target_dir lib_name =
    Output_kind.Public_library
      { lib_dir = Lib_info.src_dir info
      ; target_dir
      ; output_dir =
          Path.Local.relative
            (Path.Local.of_string "node_modules")
            (Lib_name.to_string lib_name)
      }
  in
  fun ~target_dir lib ->
    let info = Lib.info lib in
    match Lib_info.status info with
    | Private (_, None) -> Output_kind.Private_library_or_emit target_dir
    | Private (_, Some pkg) ->
      public_lib
        ~info
        ~target_dir
        (Lib_name.mangled (Package.name pkg) (Lib_name.to_local_exn (Lib.name lib)))
    | Installed | Installed_private | Public _ ->
      public_lib ~info ~target_dir (Lib_info.name info)
;;

let lib_output_path ~output_dir ~lib_dir src =
  if Path.equal lib_dir src
  then output_dir
  else (
    let src_dir = Path.drop_prefix_exn src ~prefix:lib_dir in
    Path.Build.append_local output_dir src_dir)
;;

let make_js_name ~js_ext ~output m =
  let dst_dir =
    let src_dir =
      Module.source m ~ml_kind:Impl
      |> Option.value_exn
      |> Module.File.original_path
      |> Path.parent_exn
    in
    match output with
    | Output_kind.Public_library { lib_dir; target_dir; output_dir } ->
      let output_dir = Path.Build.append_local target_dir output_dir in
      lib_output_path ~output_dir ~lib_dir src_dir
    | Private_library_or_emit target_dir ->
      Path.Build.append_source
        target_dir
        (src_dir |> Path.as_in_build_dir_exn |> Path.Build.drop_build_context_exn)
  in
  let basename = Module_compilation.melange_js_basename m ^ js_ext in
  Path.Build.relative dst_dir basename
;;

let modules_in_obj_dir ~sctx ~scope ~preprocess modules =
  let* version =
    let+ ocaml = Context.ocaml (Super_context.context sctx) in
    ocaml.version
  and* preprocess =
    Instrumentation.with_instrumentation
      preprocess
      ~instrumentation_backend:(Lib.DB.instrumentation_backend (Scope.libs scope))
    |> Resolve.Memo.read_memo
  in
  let pped_map = Staged.unstage (Pp_spec.pped_modules_map preprocess version) in
  Modules.map_user_written modules ~f:(fun m -> Memo.return @@ pped_map m)
;;

let for_ = Compilation_mode.Melange

let impl_only_modules_defined_in_this_lib ~sctx ~scope lib =
  match Lib_info.modules (Lib.info lib) ~for_ with
  | External None ->
    User_error.raise
      [ Pp.textf
          "The library %s was not compiled with Dune or it was compiled with Dune but \
           published with a META template. Such libraries are not compatible with \
           melange support"
          (Lib.name lib |> Lib_name.to_string)
      ]
  | External (Some modules) ->
    Memo.return
      ( modules
      , (Modules.With_vlib.split_by_lib modules).impl
        |> List.filter ~f:(Module.has ~ml_kind:Impl) )
  | Local ->
    let lib = Lib.Local.of_lib_exn lib in
    let info = Lib.Local.info lib in
    let+ modules =
      let* modules = Dir_contents.modules_of_local_lib sctx lib ~for_ in
      let preprocess = Lib_info.preprocess info ~for_ in
      modules_in_obj_dir ~sctx ~scope ~preprocess modules >>| Modules.With_vlib.modules
    in
    let () =
      let modes = Lib_info.modes info in
      match modes.melange with
      | false ->
        let lib_name = Lib_name.to_string (Lib_info.name info) in
        User_error.raise
          ~loc:(Lib_info.loc info)
          [ Pp.textf
              "The library `%s` was added as a dependency of a `melange.emit` stanza, \
               but this library is not compatible with Melange. To fix this, add \
               `melange` to the `modes` field of the library `%s`."
              lib_name
              lib_name
          ]
      | true -> ()
    in
    let impl_only =
      Modules.With_vlib.fold_no_vlib_with_aliases
        modules
        ~init:[]
        ~normal:(fun m acc -> if Module.has m ~ml_kind:Impl then m :: acc else acc)
        ~alias:(fun _m acc -> acc)
    in
    modules, impl_only
;;

let cmj_includes =
  let cmj_glob = Glob.of_string_exn Loc.none "*.cmj" in
  fun ~(requires_link : Lib.t list Resolve.t) ~scope lib_config ->
    let project = Scope.project scope in
    let deps_of_lib lib =
      let info = Lib.info lib in
      let obj_dir = Lib_info.obj_dir info in
      let dir = Obj_dir.melange_dir obj_dir in
      Dep.file_selector @@ File_selector.of_glob ~dir cmj_glob
    in
    Command.Args.memo
    @@ Resolve.args
    @@
    let open Resolve.O in
    let+ requires_link = requires_link in
    let deps = List.map requires_link ~f:deps_of_lib |> Dep.Set.of_list in
    Command.Args.S
      [ Lib_flags.L.melange_emission_include_flags ~project requires_link lib_config
      ; Hidden_deps deps
      ]
;;

let compile_info ~scope (mel : Melange_stanzas.Emit.t) =
  let dune_version = Scope.project scope |> Dune_project.dune_version in
  let+ pps =
    Instrumentation.with_instrumentation
      mel.preprocess
      ~instrumentation_backend:(Lib.DB.instrumentation_backend (Scope.libs scope))
    |> Resolve.Memo.read_memo
    >>| Preprocess.Per_module.pps
  in
  let libraries =
    match mel.emit_stdlib with
    | false -> mel.libraries
    | true ->
      let builtin_melange_dep = Lib_dep.Direct (mel.loc, Lib_name.of_string "melange") in
      builtin_melange_dep :: mel.libraries
  in
  Lib.DB.resolve_user_written_deps
    (Scope.libs scope)
    (`Melange_emit mel.target)
    ~allow_overlaps:mel.allow_overlapping_dependencies
    ~forbidden_libraries:[]
    libraries
    ~allow_unused_libraries:[]
    ~pps
    ~dune_version
;;

let js_targets_of_modules modules ~module_systems ~output =
  List.map module_systems ~f:(fun (_, js_ext) ->
    modules
    |> Modules.With_vlib.fold_no_vlib_with_aliases
         ~init:Path.Set.empty
         ~alias:(fun _m acc -> acc)
         ~normal:(fun m acc ->
           if Module.has m ~ml_kind:Impl
           then (
             let target = Path.build @@ make_js_name ~js_ext ~output m in
             Path.Set.add acc target)
           else acc))
  |> Path.Set.union_all
;;

let js_targets_of_libs ~sctx ~scope ~module_systems ~target_dir libs =
  Resolve.Memo.List.concat_map module_systems ~f:(fun (_, js_ext) ->
    let of_lib lib =
      let+ _, modules = impl_only_modules_defined_in_this_lib ~sctx ~scope lib in
      let output = output_of_lib ~target_dir lib in
      List.rev_map modules ~f:(fun m -> Path.build @@ make_js_name ~output ~js_ext m)
    in
    Resolve.Memo.List.concat_map libs ~f:(fun lib ->
      let* base = of_lib lib in
      match Lib.implements lib with
      | None -> Resolve.Memo.return base
      | Some vlib ->
        let open Resolve.Memo.O in
        let* vlib = vlib in
        let+ for_vlib = Resolve.Memo.lift_memo (of_lib vlib) in
        List.rev_append for_vlib base))
;;

let compute_promote_in_source ~promote_in_source ~project ~dir ~mode ~output ~src ~dst =
  match promote_in_source with
  | false -> mode
  | true ->
    (match mode with
     | Rule.Mode.Standard | Fallback | Ignore_source_files -> mode
     | Promote p ->
       let new_into_dir =
         let dir = Path.build dir in
         let src_dir = Path.parent_exn src in
         let dst_dir = Path.Build.parent_exn dst |> Path.build in
         match output with
         | Output_kind.Private_library_or_emit _ ->
           let into_dir =
             let into_dir =
               (* interpret `(into ...)` relative to the dune file, not the `target_dir` *)
               Option.map p.into ~f:(fun into -> Path.relative dir into.dir)
               |> Option.value ~default:dir
             in
             let segment =
               Path.descendant src_dir ~of_:dir
               |> Option.value_exn
               |> Path.as_in_source_tree_exn
             in
             Path.append_source into_dir segment
           in
           Path.reach ~from:dst_dir into_dir
         | Public_library { lib_dir; output_dir; target_dir = _ } ->
           let into_dir =
             let root = Dune_project.root project in
             let into_dir = Path.Source.append_local root output_dir in
             let segment = Path.drop_prefix_exn src_dir ~prefix:lib_dir in
             Path.Source.append_local into_dir segment
           in
           let from = Path.drop_build_context_exn dst_dir |> Path.source in
           Path.reach ~from (Path.source into_dir)
       in
       let into =
         let loc =
           Option.map p.into ~f:(fun x -> x.loc) |> Option.value ~default:Loc.none
         in
         Some { Rule.Promote.Into.loc; dir = new_into_dir }
       in
       Promote { p with into })
;;

let build_js
      ~loc
      ~dir
      ~scope
      ~pkg_name
      ~promote_in_source
      ~mode
      ~module_systems
      ~output
      ~obj_dir
      ~sctx
      ~includes
      ~(compile_flags : Ocaml_flags.t)
      ~local_modules_and_obj_dir
      m
  =
  let project = Scope.project scope in
  let melange_extension_version =
    Dune_project.find_extension_version project Dune_lang.Melange.syntax
    |> Option.value_exn
  in
  let* compiler = Melange_binary.melc sctx ~loc:(Some loc) ~dir in
  Memo.parallel_iter module_systems ~f:(fun (module_system, js_ext) ->
    let js_output = make_js_name ~output ~js_ext m in
    let mode =
      let src = Module.source_without_pp m ~ml_kind:Impl |> Option.value_exn in
      compute_promote_in_source
        ~promote_in_source
        ~project
        ~dir
        ~output
        ~mode
        ~src
        ~dst:js_output
    in
    let build =
      let command =
        let src = Obj_dir.Module.cm_file_exn obj_dir m ~kind:(Melange Cmj) in
        let obj_dir = [ Command.Args.A "-I"; Path (Obj_dir.melange_dir obj_dir) ] in
        let melange_package_args =
          let pkg_name_args =
            match pkg_name, melange_extension_version with
            | None, _ -> []
            | Some pkg_name, (0, 1) ->
              [ "--bs-package-name"; Package.Name.to_string pkg_name ]
            | Some pkg_name, _ ->
              [ "--mel-package-name"; Package.Name.to_string pkg_name ]
          in
          let js_modules_str = Melange.Module_system.to_string module_system in
          (if melange_extension_version >= (1, 0)
           then "--mel-module-type"
           else "--bs-module-type")
          :: js_modules_str
          :: pkg_name_args
        in
        Command.run
          ~dir:(Super_context.context sctx |> Context.build_dir |> Path.build)
          compiler
          [ Command.Args.S obj_dir
          ; Command.Args.as_any includes
          ; Command.Args.dyn (Ocaml_flags.get compile_flags Melange)
          ; As melange_package_args
          ; A "-o"
          ; Target js_output
          ; Dep src
          ]
      in
      match local_modules_and_obj_dir with
      | Some (modules, obj_dir) ->
        With_targets.map_build command ~f:(fun command ->
          let open Action_builder.O in
          let paths =
            let+ module_deps =
              Dep_rules.read_deps_of ~obj_dir ~modules ~ml_kind:Impl m ~for_
            in
            List.filter_map module_deps ~f:(fun dep_m ->
              let kind : Lib_mode.Cm_kind.t = Melange Cmj in
              Obj_dir.Module.cm_file obj_dir dep_m ~kind |> Option.map ~f:Path.build)
          in
          Action_builder.dyn_paths_unit paths >>> command)
      | None -> command
    in
    Super_context.add_rule sctx ~dir ~loc ~mode build)
;;

(* attach [deps] to the specified [alias] AND the (dune default) [all] alias.

   when [alias] is not supplied, {!Melange_stanzas.Emit.implicit_alias} is
   assumed. *)
let add_deps_to_aliases ?(alias = Melange_stanzas.Emit.implicit_alias) ~dir deps =
  let alias = Alias.make alias ~dir in
  let dune_default_alias = Alias.make Alias0.all ~dir in
  let attach alias = Rules.Produce.Alias.add_deps alias deps in
  Memo.parallel_iter ~f:attach [ alias; dune_default_alias ]
;;

let melange_compile_flags ~sctx ~dir (mel : Melange_stanzas.Emit.t) =
  let common = Ordered_set_lang.Unexpanded.standard in
  let specific =
    let ocaml = Mode.Dict.make_both common in
    { Lib_mode.Map.ocaml; melange = mel.compile_flags }
  in
  Dune_lang.Ocaml_flags.Spec.make ~common ~specific
  |> Ocaml_flags_db.ocaml_flags sctx ~dir
  >>| Ocaml_flags.allow_only_melange
;;

let for_ = Compilation_mode.Melange

let setup_emit_cmj_rules
      ~sctx
      ~scope
      ~expander
      ~dir_contents
      (mel : Melange_stanzas.Emit.t)
  =
  let* compile_info = compile_info ~scope mel in
  let ctx = Super_context.context sctx in
  let merlin_ident = Merlin_ident.for_melange ~target:mel.target in
  let dir = Dir_contents.dir dir_contents in
  let f () =
    let* modules, obj_dir =
      Dir_contents.melange dir_contents
      >>= Ml_sources.modules_and_obj_dir
            ~libs:(Scope.libs scope)
            ~for_:(Melange { target = mel.target })
    in
    let* () = Check_rules.add_obj_dir sctx ~obj_dir for_ in
    let* modules, pp =
      let+ modules, pp =
        Buildable_rules.modules_rules
          sctx
          (Melange
             { preprocess = mel.preprocess
             ; preprocessor_deps = mel.preprocessor_deps
             ; lint = mel.lint
             ; (* why is this always false? *)
               empty_module_interface_if_absent = false
             })
          expander
          ~dir
          scope
          modules
      in
      Modules.With_vlib.modules modules, pp
    in
    let requires_link = Lib.Compile.requires_link compile_info ~for_ in
    let* flags = melange_compile_flags ~sctx ~dir mel in
    let* cctx =
      let direct_requires = Lib.Compile.direct_requires compile_info ~for_ in
      Compilation_context.create
        for_
        ~loc:mel.loc
        ~super_context:sctx
        ~scope
        ~obj_dir
        ~modules
        ~flags
        ~requires_link
        ~requires_compile:direct_requires
        ~preprocessing:pp
        ~js_of_ocaml:(Js_of_ocaml.Mode.Pair.make None)
        ~opaque:Inherit_from_settings
        ~melange_package_name:None
        ~package:mel.package
        ~modes:{ ocaml = Mode.Dict.make_both false; melange = true }
    in
    let* () = Module_compilation.build_all cctx in
    let* () =
      Memo.when_ (Compilation_context.bin_annot cctx) (fun () ->
        Ocaml_index.cctx_rules cctx)
    in
    let* requires_compile = Compilation_context.requires_compile cctx in
    let* requires_hidden = Compilation_context.requires_hidden cctx in
    let stdlib_dir = (Compilation_context.ocaml cctx).lib_config.stdlib_dir in
    let+ () =
      let emit_and_libs_deps =
        let target_dir = Melange_stanzas.Emit.target_dir ~dir mel in
        let module_systems = mel.module_systems in
        let open Action_builder.O in
        let+ () =
          js_targets_of_modules
            ~output:(Private_library_or_emit target_dir)
            ~module_systems
            modules
          |> Action_builder.path_set
        and+ () =
          let* deps =
            Resolve.Memo.read
            @@
            let open Resolve.Memo.O in
            Compilation_context.requires_link cctx
            >>= js_targets_of_libs ~sctx ~scope ~module_systems ~target_dir
          in
          Action_builder.paths deps
        in
        ()
      in
      add_deps_to_aliases ?alias:mel.alias emit_and_libs_deps ~dir
    in
    ( cctx
    , Merlin.make
        ~requires_compile
        ~requires_hidden
        ~stdlib_dir
        ~flags
        ~modules
        ~libname:None
        ~preprocess:(Preprocess.Per_module.without_instrumentation mel.preprocess)
        ~obj_dir
        ~ident:merlin_ident
        ~dialects:(Dune_project.dialects (Scope.project scope))
        ~for_
        ~parameters:(Resolve.return []) )
  in
  let* () = Buildable_rules.gen_select_rules sctx compile_info ~dir ~for_ in
  Buildable_rules.with_lib_deps ctx merlin_ident ~dir ~f
;;

module Runtime_deps = struct
  type targets =
    { copy : (Path.t * Path.Build.t) list
    ; deps : Path.t list
    }

  let empty = { copy = []; deps = [] }

  let targets =
    let raise_external_dep_error src ~for_ =
      let lib_info =
        match for_ with
        | `Library lib_info -> lib_info
        | `Emit -> assert false
      in
      let loc =
        match Lib_info.melange_runtime_deps lib_info with
        | Local (loc, _) -> loc
        | External _ -> assert false
      in
      Lib_file_deps.raise_disallowed_external_path ~loc (Lib_info.name lib_info) src
    in
    fun sctx ~dir ~output ~for_ (mel : Melange_stanzas.Emit.t) ->
      let+ deps =
        match for_ with
        | `Emit ->
          let* expander = Super_context.expander sctx ~dir in
          let loc, runtime_deps = mel.runtime_deps in
          Lib_file_deps.eval ~expander ~loc ~paths:Allow_all runtime_deps
        | `Library lib_info ->
          (match Lib_info.melange_runtime_deps lib_info with
           | External paths -> Memo.return (Path.Set.of_list paths)
           | Local (loc, dep_conf) ->
             let dir = Lib_info.src_dir (Lib_info.as_local_exn lib_info) in
             let* expander = Super_context.expander sctx ~dir in
             Lib_file_deps.eval ~expander ~loc ~paths:Allow_all dep_conf)
      in
      match output with
      | Output_kind.Public_library { lib_dir; target_dir; output_dir } ->
        Path.Set.fold ~init:empty deps ~f:(fun src ({ copy; deps = _ } as acc) ->
          let copy =
            match Path.as_external src with
            | None ->
              let output_dir = Path.Build.append_local target_dir output_dir in
              (src, lib_output_path ~output_dir ~lib_dir src) :: copy
            | Some src_e ->
              (match Path.as_external lib_dir with
               | Some lib_dir_e when Path.External.is_descendant src_e ~of_:lib_dir_e ->
                 let output_dir = Path.Build.append_local target_dir output_dir in
                 (src, lib_output_path ~output_dir ~lib_dir src) :: copy
               | Some _ | None -> raise_external_dep_error src ~for_)
          in
          { acc with copy })
      | Private_library_or_emit output_dir ->
        Path.Set.fold ~init:empty deps ~f:(fun src ({ copy; deps } as acc) ->
          match Path.as_in_build_dir src with
          | None -> { acc with deps = src :: deps }
          | Some src_build ->
            let target = Path.Build.drop_build_context_exn src_build in
            { acc with copy = (src, Path.Build.append_source output_dir target) :: copy })
  ;;
end

let setup_runtime_assets_rules
      sctx
      ~scope
      ~dir
      ~target_dir
      ~mode
      ~promote_in_source
      ~output
      ~for_
      mel
  =
  Runtime_deps.targets sctx ~dir ~output ~for_ mel
  >>= fun { Runtime_deps.copy; deps } ->
  let loc = mel.loc in
  Memo.parallel_map copy ~f:(fun (src, dst) ->
    let mode =
      compute_promote_in_source
        ~promote_in_source
        ~project:(Scope.project scope)
        ~dir
        ~output
        ~mode
        ~src
        ~dst
    in
    Memo.Option.bind
      (Path.as_in_build_dir src)
      ~f:(Dir_status.find_directory_target_ancestor ~jsoo_enabled:Jsoo_rules.jsoo_enabled)
    >>= function
    | None ->
      Memo.Option.map (Path.as_outside_build_dir src) ~f:Fs_memo.is_directory
      >>= fun is_dir ->
      let dst, builder =
        match is_dir with
        | Some (Ok true) -> Right dst, Action_builder.symlink_dir ~src ~dst
        | Some (Ok false) | Some (Error _) | None ->
          Left dst, Action_builder.copy ~src ~dst
      in
      let+ () = Super_context.add_rule ~loc ~dir ~mode sctx builder in
      dst
    | Some directory_target_ancestor ->
      let new_src = Path.build directory_target_ancestor in
      let dst =
        let rel = Path.reach ~from:src new_src in
        Path.Build.relative dst rel
      in
      let builder = Action_builder.symlink_dir ~src:new_src ~dst in
      let+ () = Super_context.add_rule ~loc ~dir ~mode sctx builder in
      Right dst)
  >>| List.partition_map ~f:Fun.id
  >>= fun (file_deps, directory_targets) ->
  let+ () =
    let paths =
      List.concat_map [ file_deps; directory_targets ] ~f:(List.map ~f:Path.build) @ deps
    in
    add_deps_to_aliases ?alias:mel.alias (Action_builder.paths paths) ~dir:target_dir
  in
  Path.Build.Map.of_list_map_exn directory_targets ~f:(fun p -> p, loc)
;;

let modules_for_js_and_obj_dir ~sctx ~dir_contents ~scope (mel : Melange_stanzas.Emit.t) =
  let* modules, obj_dir =
    Dir_contents.melange dir_contents
    >>= Ml_sources.modules_and_obj_dir
          ~libs:(Scope.libs scope)
          ~for_:(Melange { target = mel.target })
  in
  let+ modules = modules_in_obj_dir ~sctx ~scope ~preprocess:mel.preprocess modules in
  let modules_for_js =
    Modules.fold_user_available modules ~init:[] ~f:(fun x acc ->
      if Module.has x ~ml_kind:Impl then x :: acc else acc)
  in
  modules, modules_for_js, obj_dir
;;

let should_promote_in_source scope =
  let project = Scope.project scope in
  match Dune_project.find_extension_version project Dune_lang.Melange.syntax with
  | Some v -> v >= (1, 0)
  | None -> false
;;

let setup_entries_js
      ~sctx
      ~dir
      ~dir_contents
      ~scope
      ~requires_link
      ~target_dir
      ~mode
      (mel : Melange_stanzas.Emit.t)
  =
  let* local_modules, modules_for_js, local_obj_dir =
    modules_for_js_and_obj_dir ~sctx ~dir_contents ~scope mel
  in
  let pkg_name = Option.map mel.package ~f:Package.name in
  let loc = mel.loc in
  let module_systems = mel.module_systems in
  let* includes =
    let+ lib_config =
      let+ ocaml = Super_context.context sctx |> Context.ocaml in
      ocaml.lib_config
    in
    let requires_link = Resolve.return requires_link in
    cmj_includes ~requires_link ~scope lib_config
  and* compile_flags = melange_compile_flags ~sctx ~dir mel in
  let output = Output_kind.Private_library_or_emit target_dir in
  let obj_dir = Obj_dir.of_local local_obj_dir in
  let promote_in_source = should_promote_in_source scope in
  let local_modules_and_obj_dir =
    Some (Modules.With_vlib.modules local_modules, local_obj_dir)
  in
  let+ directory_targets =
    setup_runtime_assets_rules
      sctx
      ~scope
      ~dir
      ~target_dir
      ~mode
      ~promote_in_source
      ~output
      ~for_:`Emit
      mel
  and+ () =
    Memo.parallel_iter modules_for_js ~f:(fun m ->
      build_js
        ~loc
        ~dir
        ~scope
        ~pkg_name
        ~promote_in_source
        ~mode
        ~module_systems
        ~output
        ~obj_dir
        ~sctx
        ~includes
        ~compile_flags
        ~local_modules_and_obj_dir
        m)
  in
  directory_targets
;;

let setup_js_rules_libraries =
  let local_modules_and_obj_dir ~lib modules =
    Lib.Local.of_lib lib
    |> Option.map ~f:(fun lib ->
      let obj_dir = Lib.Local.obj_dir lib in
      modules, obj_dir)
  in
  let parallel_build_source_modules ~sctx ~scope ~f:build_js lib =
    let* local_modules_and_obj_dir, source_modules =
      let+ lib_modules, source_modules =
        impl_only_modules_defined_in_this_lib ~sctx ~scope lib
      in
      local_modules_and_obj_dir ~lib lib_modules, source_modules
    in
    Memo.parallel_iter source_modules ~f:(build_js ~local_modules_and_obj_dir)
  in
  fun ~dir ~scope ~target_dir ~sctx ~requires_link ~mode (mel : Melange_stanzas.Emit.t) ->
    let build_js = build_js ~sctx ~scope ~mode ~module_systems:mel.module_systems in
    let with_vlib_implementations =
      let vlib_implementations =
        (* vlib_name => concrete_impl *)
        List.fold_left requires_link ~init:Lib_name.Map.empty ~f:(fun acc dep ->
          match Lib_info.implements (Lib.info dep) with
          | None -> acc
          | Some (_, vlib_name) -> Lib_name.Map.add_exn acc vlib_name dep)
      in
      fun lib deps ->
        (* Depend on the concrete implementations of virtual libraries so
           that Melange can find their `.cmj` files. *)
        List.fold_left deps ~init:deps ~f:(fun acc dep ->
          match Lib_name.Map.find vlib_implementations (Lib.name dep) with
          | None -> acc
          | Some sub -> if Lib.equal sub lib then acc else sub :: acc)
    in
    let* lib_config =
      let+ ocaml = Super_context.context sctx |> Context.ocaml in
      ocaml.lib_config
    in
    let+ dir_targets =
      Memo.parallel_map requires_link ~f:(fun lib ->
        let lib_compile_info =
          Lib.Compile.for_lib
            ~allow_overlaps:mel.allow_overlapping_dependencies
            (Scope.libs scope)
            lib
        in
        let info = Lib.info lib in
        let promote_in_source = should_promote_in_source scope in
        let build_js =
          let loc = Lib_info.loc info in
          let obj_dir = Lib_info.obj_dir info in
          let pkg_name = Lib_info.package info in
          build_js ~loc ~promote_in_source ~pkg_name ~obj_dir
        in
        let output = output_of_lib ~target_dir lib in
        let* includes =
          let+ requires_link =
            Memo.Lazy.force (Lib.Compile.requires_link lib_compile_info ~for_)
            |> Resolve.Memo.map ~f:(with_vlib_implementations lib)
          in
          cmj_includes ~requires_link ~scope lib_config
        and* compile_flags = melange_compile_flags ~sctx ~dir mel in
        let+ directory_targets =
          setup_runtime_assets_rules
            sctx
            ~scope
            ~dir
            ~target_dir
            ~mode
            ~promote_in_source
            ~output
            ~for_:(`Library info)
            mel
        and+ () =
          match Lib.implements lib with
          | None -> Memo.return ()
          | Some vlib ->
            let* vlib = Resolve.Memo.read_memo vlib in
            let vlib_output = output_of_lib ~target_dir vlib in
            (match vlib_output, output with
             | Public_library _, Private_library_or_emit _ ->
               let info = Lib.info lib in
               User_error.raise
                 ~loc:(Lib_info.loc info)
                 [ Pp.text
                     "Dune doesn't currently support building private implementations of \
                      virtual public libaries for `(modes melange)`"
                 ]
                 ~hints:
                   [ Pp.textf
                       "Add a `public_name` to the library `%s'."
                       (Lib_name.to_string (Lib_info.name info))
                   ]
             | Public_library _, Public_library _ | Private_library_or_emit _, _ ->
               let* includes =
                 let+ requires_link =
                   let+ requires_link =
                     Lib.Compile.for_lib
                       ~allow_overlaps:mel.allow_overlapping_dependencies
                       (Scope.libs scope)
                       vlib
                     |> Lib.Compile.requires_link ~for_
                     |> Memo.Lazy.force
                   in
                   let open Resolve.O in
                   let+ requires_link = requires_link in
                   (* Whenever a `concrete_lib` implementation contains a field
                    `(implements virt_lib)`, we also set up the JS targets for the
                    modules defined in `virt_lib`.

                    In the cases where `virt_lib` (concrete) modules depend on any
                    virtual modules (i.e. programming against the interface), we
                    need to make sure that the JS rules that dune emits for
                    `virt_lib` depend on `concrete_lib`, such that Melange can find
                    the correct `.cmj` file, which is needed to emit the correct
                    path in `import` / `require`. *)
                   lib :: requires_link
                 in
                 cmj_includes ~requires_link ~scope lib_config
               in
               parallel_build_source_modules
                 ~sctx
                 ~scope
                 vlib
                 ~f:(build_js ~dir ~output:vlib_output ~includes ~compile_flags))
        and+ () =
          parallel_build_source_modules
            ~sctx
            ~scope
            lib
            ~f:(build_js ~dir ~output ~includes ~compile_flags)
        in
        directory_targets)
    in
    List.fold_left dir_targets ~init:Path.Build.Map.empty ~f:(fun acc dir_targets ->
      Path.Build.Map.merge acc dir_targets ~f:(fun _ l1 l2 ->
        match l1, l2 with
        | None, None -> None
        | Some loc, None | None, Some loc -> Some loc
        | Some _, Some _ -> assert false))
;;

let setup_js_rules_libraries_and_entries
      ~dir_contents
      ~dir
      ~scope
      ~sctx
      ~requires_link
      ~mode
      ~target_dir
      mel
  =
  let+ dir_targets_libraries =
    setup_js_rules_libraries ~dir ~scope ~target_dir ~sctx ~requires_link ~mode mel
  and+ directory_targets =
    setup_entries_js ~sctx ~dir ~dir_contents ~scope ~requires_link ~target_dir ~mode mel
  in
  Path.Build.Map.merge
    dir_targets_libraries
    directory_targets
    ~f:(fun _ lib_dir emit_dir ->
      match lib_dir, emit_dir with
      | None, None -> None
      | Some loc, None | None, Some loc -> Some loc
      | Some _, Some _ -> assert false)
;;

let setup_emit_js_rules ~dir_contents ~dir ~scope ~sctx mel =
  let target_dir =
    let dir = Dir_contents.dir dir_contents in
    Melange_stanzas.Emit.target_dir ~dir mel
  in
  let* mode =
    let* expander = Super_context.expander sctx ~dir in
    let mode =
      match mel.promote with
      | None -> Rule_mode.Standard
      | Some p -> Promote p
    in
    match should_promote_in_source scope with
    | true -> Rule_mode_expand.expand_path ~expander ~dir mode
    | false -> Rule_mode_expand.expand_str ~expander mode
  in
  let* compile_info = compile_info ~scope mel in
  let* requires_link_resolve =
    Lib.Compile.requires_link compile_info ~for_ |> Memo.Lazy.force
  in
  match Resolve.to_result requires_link_resolve with
  | Ok requires_link ->
    setup_js_rules_libraries_and_entries
      ~dir_contents
      ~dir
      ~scope
      ~sctx
      ~requires_link
      ~mode
      ~target_dir
      mel
  | Error resolve_error ->
    (* NOTE: in multi-package projects where [melange.emit] stanzas are
       present, we can't eagerly resolve the link-time closure for
       [melange.emit] stanzas since their targets aren't public (i.e. part of a
       package). When resolution fails, we replace the JS entries with the
       resolution error inside [Action_builder.fail] to give Dune a chance to
       fail if any of the targets end up attached to a package installation. *)
    let* _, modules_for_js, _obj_dir =
      modules_for_js_and_obj_dir ~sctx ~dir_contents ~scope mel
    in
    let module_systems = mel.module_systems in
    let output = Output_kind.Private_library_or_emit target_dir in
    let loc = mel.loc in
    let+ () =
      Memo.parallel_iter modules_for_js ~f:(fun m ->
        Memo.parallel_iter module_systems ~f:(fun (_module_system, js_ext) ->
          let file_targets = [ make_js_name ~output ~js_ext m ] in
          Super_context.add_rule
            sctx
            ~dir
            ~loc
            ~mode
            (Action_builder.fail
               { fail = (fun () -> Resolve.raise_error_with_stack_trace resolve_error) }
             |> Action_builder.with_file_targets ~file_targets)))
    in
    Path.Build.Map.empty
;;

(* The emit stanza of melange outputs in a single output directory (and its
   descendants). We attach all .js generating rules to this root directory.

   Since we allow user defined rules in this output directory, we need to know
   when we're under the emit directory so that we load both the user defined
   rules and the rules originating from the emit stanza. *)
type t =
  { (* the directory in which the emit stanza is defined. *)
    stanza_dir : Path.Build.t
  ; (* the emit stanza itself. *)
    stanza : Melange_stanzas.Emit.t
  }

let emit_rules sctx { stanza_dir; stanza } =
  Rules.collect (fun () ->
    let* sctx = sctx in
    let* dir_contents = Dir_contents.get sctx ~dir:stanza_dir in
    let* scope = Scope.DB.find_by_dir stanza_dir in
    setup_emit_js_rules ~dir_contents ~dir:stanza_dir ~scope ~sctx stanza)
;;

(* Detect if [dir] is under the target directory of a melange.emit stanza. *)
let rec under_melange_emit_target ~sctx ~dir =
  match Path.Build.parent dir with
  | None -> Memo.return None
  | Some parent ->
    Dune_load.stanzas_in_dir parent
    >>= (function
     | None -> under_melange_emit_target ~sctx ~dir:parent
     | Some stanzas ->
       Dune_file.find_stanzas stanzas Melange_stanzas.Emit.key
       >>= Memo.List.find_map ~f:(fun (mel : Melange_stanzas.Emit.t) ->
         let target_dir = Melange_stanzas.Emit.target_dir ~dir:parent mel in
         match Path.Build.equal target_dir dir with
         | false -> Memo.return None
         | true ->
           (* In the case where we have two melange.emit stanzas in the same folder,
              with one enabled in the current context and one disabled, we want to
              make sure that we pick the enabled one *)
           let+ enabled =
             let* expander =
               let* sctx = sctx in
               Super_context.expander sctx ~dir
             in
             Expander.eval_blang expander mel.enabled_if
           in
           Option.some_if enabled mel)
       >>= (function
        | None -> under_melange_emit_target ~sctx ~dir:parent
        | Some stanza -> Memo.return @@ Some { stanza_dir = parent; stanza }))
;;

let gen_emit_rules sctx ~dir ({ stanza_dir; stanza } as for_melange) =
  match Path.Build.equal dir (Melange_stanzas.Emit.target_dir ~dir:stanza_dir stanza) with
  | false -> Memo.return None
  | true ->
    under_melange_emit_target ~sctx ~dir:stanza_dir
    >>| (function
     | None -> Some (emit_rules sctx for_melange)
     | Some { stanza_dir = _; stanza = parent_stanza } ->
       let main_message = Pp.text "melange.emit stanzas cannot be nested" in
       let annots =
         let main = User_message.make ~loc:stanza.loc [ main_message ] in
         let related =
           [ User_message.make
               ~loc:parent_stanza.loc
               [ Pp.text "under this melange stanza" ]
           ]
         in
         User_message.Annots.singleton
           Compound_user_error.annot
           [ Compound_user_error.make ~main ~related ]
       in
       User_error.raise
         ~loc:stanza.loc
         ~annots
         [ main_message
         ; Pp.enumerate ~f:Loc.pp_file_colon_line [ parent_stanza.loc; stanza.loc ]
         ]
         ~hints:[ Pp.text "Move both `melange.emit' stanzas to the same level." ])
;;

module Gen_rules = Build_config.Gen_rules

let setup_emit_js_rules sctx ~dir =
  under_melange_emit_target ~sctx ~dir
  >>= function
  | Some melange ->
    gen_emit_rules sctx ~dir melange
    >>= (function
     | None -> Memo.return (Gen_rules.redirect_to_parent Gen_rules.Rules.empty)
     | Some melange ->
       let+ directory_targets, melange = melange in
       Gen_rules.make ~directory_targets (Memo.return melange))
  | None ->
    (* this should probably be handled by [Dir_status] *)
    Dune_load.stanzas_in_dir dir
    >>= (function
     | None -> Memo.return Gen_rules.no_rules
     | Some dune_file ->
       let+ build_dir_only_sub_dirs =
         Dune_file.find_stanzas dune_file Melange_stanzas.Emit.key
         >>| List.map ~f:(fun (mel : Melange_stanzas.Emit.t) -> mel.target)
         >>| Subdir_set.of_list
         >>| Gen_rules.Build_only_sub_dirs.singleton ~dir
       in
       Gen_rules.make ~build_dir_only_sub_dirs (Memo.return Rules.empty))
;;
