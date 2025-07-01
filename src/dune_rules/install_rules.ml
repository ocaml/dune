open Import
open Memo.O

let install_file ~(package : Package.Name.t) ~findlib_toolchain =
  let package = Package.Name.to_string package in
  match findlib_toolchain with
  | None -> package ^ ".install"
  | Some x -> sprintf "%s-%s.install" package (Context_name.to_string x)
;;

let need_odoc_config (pkg : Package.t) =
  match Package.documentation pkg with
  | { packages = [] } -> false
  | _ -> true
;;

module Package_paths = struct
  let opam_file (ctx : Build_context.t) (pkg : Package.t) =
    let opam_file = Package.opam_file pkg in
    let exists =
      match Package.has_opam_file pkg with
      | Exists b -> b
      | Generated -> true
    in
    if exists then Some (Path.Build.append_source ctx.build_dir opam_file) else None
  ;;

  let meta_fn pkg = "META." ^ Package.Name.to_string pkg

  let deprecated_meta_file (ctx : Build_context.t) pkg name =
    let meta_fn = meta_fn name in
    let pkg_dir = Package.dir pkg in
    Path.Build.append_source ctx.build_dir (Path.Source.relative pkg_dir meta_fn)
  ;;

  let meta_file ctx pkg =
    let name = Package.name pkg in
    deprecated_meta_file ctx pkg name
  ;;

  let build_dir (ctx : Build_context.t) (pkg : Package.t) =
    let dir = Package.dir pkg in
    Path.Build.append_source ctx.build_dir dir
  ;;

  let dune_package_file ctx pkg =
    let name = Package.name pkg in
    Path.Build.relative (build_dir ctx pkg) (Package.Name.to_string name ^ ".dune-package")
  ;;

  let odoc_config_file ctx pkg =
    if need_odoc_config pkg
    then (
      let name = Package.name pkg in
      Some
        (Path.Build.relative
           (build_dir ctx pkg)
           (Package.Name.to_string name ^ ".odoc-config.sexp")))
    else None
  ;;

  let deprecated_dune_package_file ctx pkg name =
    Path.Build.relative (build_dir ctx pkg) (Package.Name.to_string name ^ ".dune-package")
  ;;

  let meta_template ctx pkg =
    Path.Build.extend_basename (meta_file ctx pkg) ~suffix:".template"
  ;;
end

let check_runtime_deps_relative_path local_path ~loc ~lib_info =
  let lib_src_dir = Lib_info.src_dir lib_info in
  match Path.Local.descendant local_path ~of_:(Path.Build.local lib_src_dir) with
  | None ->
    User_error.raise
      ~loc
      [ Pp.textf
          "Public library `%s' depends on assets outside its source tree. This is not \
           allowed."
          (lib_info |> Lib_info.name |> Lib_name.to_string)
      ]
      ~hints:
        [ Pp.textf
            "Move the offending dependency somewhere inside `%s'."
            (Path.build lib_src_dir
             |> Path.drop_optional_build_context_src_exn
             |> Path.Source.to_string)
        ]
  | Some _ -> ()
;;

module Stanzas_to_entries : sig
  val stanzas_to_entries
    :  Super_context.t
    -> Install.Entry.Sourced.t list Package.Name.Map.t Memo.t
end = struct
  let lib_ppxs ctx ~scope ~(lib : Library.t) =
    match lib.kind with
    | Virtual | Parameter | Dune_file (Normal | Ppx_deriver _) -> Memo.return []
    | Dune_file (Ppx_rewriter _) ->
      Library.best_name lib
      |> Ppx_driver.ppx_exe ctx ~scope
      |> Resolve.Memo.read_memo
      >>| List.singleton
  ;;

  let lib_files ~scope ~dir_contents ~dir ~lib_config lib =
    let+ modules =
      let* ml_sources = Dir_contents.ocaml dir_contents in
      Ml_sources.modules
        ml_sources
        ~libs:(Scope.libs scope)
        ~for_:(Library (Lib_info.lib_id lib |> Lib_id.to_local_exn))
      >>| Modules.With_vlib.modules
      >>| Option.some
    and+ foreign_archives =
      match Lib_info.kind lib with
      | Dune_file _ ->
        Memo.return (Mode.Map.Multi.to_flat_list @@ Lib_info.foreign_archives lib)
      | Virtual ->
        let+ foreign_sources = Dir_contents.foreign_sources dir_contents in
        let name = Lib_info.name lib in
        let files = Foreign_sources.for_lib foreign_sources ~name in
        let { Lib_config.ext_obj; _ } = lib_config in
        Foreign.Sources.object_files files ~dir ~ext_obj
      | Parameter -> Memo.return []
    in
    List.rev_append
      (List.rev_concat_map
         ~f:(List.rev_map ~f:(fun f -> Section.Lib, f))
         (let { Mode.Dict.byte; native } = Lib_info.archives lib in
          let jsoo_files =
            (* A same runtime file can be used both for jsoo and wasmoo *)
            Lib_info.jsoo_runtime lib
            |> List.rev_append (Lib_info.wasmoo_runtime lib)
            |> List.sort_uniq ~compare:Path.Build.compare
          in
          [ byte
          ; native
          ; foreign_archives
          ; Lib_info.eval_native_archives_exn lib ~modules
          ; jsoo_files
          ]))
      (List.rev_map ~f:(fun f -> Section.Libexec, f) (Lib_info.plugins lib).native)
  ;;

  let dll_files ~(modes : Mode.Dict.Set.t) ~dynlink ~(ctx : Context.t) lib =
    (match modes.byte with
     | false -> Memo.return false
     | true ->
       let+ ocaml = Context.ocaml ctx
       and+ dynamically_linked_foreign_archives =
         Context.dynamically_linked_foreign_archives ctx
       in
       Dynlink_supported.get_ocaml_config dynlink ocaml.ocaml_config
       && dynamically_linked_foreign_archives)
    >>| function
    | false -> []
    | true -> Lib_info.foreign_dll_files lib
  ;;

  let make_entry lib_subdir =
    let in_sub_dir = function
      | None -> lib_subdir
      | Some subdir ->
        Some
          (match lib_subdir with
           | None -> subdir
           | Some lib_subdir -> Filename.concat lib_subdir subdir)
    in
    fun section ~loc ?sub_dir ?dst fn ->
      let entry =
        Install.Entry.make
          section
          fn
          ~kind:`File
          ~dst:
            (let dst =
               match dst with
               | Some s -> s
               | None -> Path.Build.basename fn
             in
             match in_sub_dir sub_dir with
             | None -> dst
             | Some dir -> sprintf "%s/%s" dir dst)
      in
      Install.Entry.Sourced.create ~loc entry
  ;;

  let doc_install_files ~loc mld_contents =
    List.rev_map mld_contents ~f:(fun (mld : Doc_sources.mld) ->
      Install.Entry.make
        ~kind:`File
        ~dst:(sprintf "odoc-pages/%s" (Path.Local.to_string mld.in_doc))
        Section.Doc
        mld.path
      |> Install.Entry.Sourced.create ~loc)
  ;;

  let lib_install_files
        sctx
        ~scope
        ~dir_contents
        ~dir
        ~sub_dir:lib_subdir
        (lib : Library.t)
    =
    let loc = lib.buildable.loc in
    let ctx = Super_context.context sctx in
    let* lib_config =
      let+ ocaml = Context.ocaml ctx in
      ocaml.lib_config
    in
    let make_entry ?(loc = loc) = make_entry lib_subdir ~loc in
    let* expander = Super_context.expander sctx ~dir in
    let info =
      Library.to_lib_info
        lib
        ~expander:(Memo.return (Expander.to_expander0 expander))
        ~dir
        ~lib_config
    in
    let lib_name = Library.best_name lib in
    let* installable_modules =
      let+ modules =
        Dir_contents.ocaml dir_contents
        >>= Ml_sources.modules
              ~libs:(Scope.libs scope)
              ~for_:(Library (Lib_info.lib_id info |> Lib_id.to_local_exn))
      and+ impl = Virtual_rules.impl sctx ~lib ~scope in
      Virtual_rules.impl_modules impl modules |> Modules.With_vlib.split_by_lib
    in
    let lib_src_dir = Lib_info.src_dir info in
    let sources =
      List.rev_concat_map installable_modules.impl ~f:(fun m ->
        List.rev_map (Module.sources m) ~f:(fun source ->
          (* We add the -gen suffix to a few files generated by dune, such
             as the alias module. *)
          let source = Path.as_in_build_dir_exn source in
          let sub_dir, dst =
            match Module.install_as m with
            | Some p ->
              let subdir =
                let parent = Path.Local.parent_exn p in
                if Path.Local.is_root parent
                then None
                else Some (Path.Local.explode parent |> String.concat ~sep:"/")
              in
              subdir, Some (Path.Local.basename p)
            | None ->
              let dst = Path.Build.basename source |> String.drop_suffix ~suffix:"-gen" in
              let sub_dir =
                let src_dir = Path.Build.parent_exn source in
                if Path.Build.equal src_dir lib_src_dir
                then None
                else
                  Path.Build.local src_dir
                  |> Path.Local.descendant ~of_:(Path.Build.local lib_src_dir)
                  |> Option.map ~f:Path.Local.to_string
              in
              sub_dir, dst
          in
          make_entry ?sub_dir Lib source ?dst))
    in
    let additional_deps =
      let find_directory_target_ancestor =
        Dir_status.find_directory_target_ancestor ~jsoo_enabled:Jsoo_rules.jsoo_enabled
      in
      fun (loc, deps) ->
        Lib_file_deps.eval deps ~expander ~loc ~paths:(Disallow_external lib_name)
        >>| Path.Set.to_list
        >>= Memo.parallel_map ~f:(fun path ->
          let path =
            let path = path |> Path.as_in_build_dir_exn in
            check_runtime_deps_relative_path ~lib_info:info ~loc (Path.Build.local path);
            path
          in
          let sub_dir =
            let src_dir = Path.Build.parent_exn path in
            match Path.Build.equal lib_src_dir src_dir with
            | true -> None
            | false ->
              Path.Build.local src_dir
              |> Path.Local.descendant ~of_:(Path.Build.local lib_src_dir)
              |> Option.map ~f:Path.Local.to_string
          in
          find_directory_target_ancestor path
          >>| function
          | None -> make_entry ?sub_dir Lib path
          | Some dir_target_path ->
            let dep = make_entry ?sub_dir Lib dir_target_path in
            { dep with entry = Install.Entry.set_kind dep.entry `Directory })
    in
    let { Lib_config.has_native; ext_obj; _ } = lib_config in
    let { Lib_mode.Map.ocaml = { Mode.Dict.byte; native } as ocaml; melange } =
      Mode_conf.Lib.Set.eval lib.modes ~has_native
    in
    let+ melange_runtime_entries = additional_deps lib.melange_runtime_deps
    and+ public_headers = additional_deps lib.public_headers
    and+ module_files =
      let obj_dir = Lib_info.obj_dir info in
      let cm_dir =
        let external_obj_dir =
          Obj_dir.convert_to_external obj_dir ~dir:(Path.build dir)
        in
        fun m cm_kind ->
          let visibility = Module.visibility m in
          let dir' = Obj_dir.cm_dir external_obj_dir cm_kind visibility in
          if Path.equal (Path.build dir) dir'
          then None
          else
            Path.drop_prefix_exn dir' ~prefix:(Path.build dir)
            |> Path.Local.to_string
            |> Option.some
      in
      let if_ b (cm_kind, f) =
        if b
        then (
          match f with
          | None -> []
          | Some f -> [ cm_kind, f ])
        else []
      in
      let common m =
        let cm_file kind = Obj_dir.Module.cm_file obj_dir m ~kind in
        let open Lib_mode.Cm_kind in
        let cmi = if_ (native || byte) (Ocaml Cmi, cm_file (Ocaml Cmi)) in
        let common_module_impls virtual_only =
          (if_ native (Ocaml Cmx, cm_file (Ocaml Cmx)) :: virtual_only)
          @ [ if_ melange (Melange Cmi, cm_file (Melange Cmi))
            ; if_ melange (Melange Cmj, cm_file (Melange Cmj))
            ]
        in
        let rest =
          match (lib.kind : Lib_kind.t) with
          | Parameter -> []
          | Virtual ->
            common_module_impls
              [ if_ byte (Ocaml Cmo, cm_file (Ocaml Cmo))
              ; if_ native (Ocaml Cmx, Obj_dir.Module.o_file obj_dir m ~ext_obj)
              ]
          | _ -> common_module_impls []
        in
        cmi :: rest |> List.rev_concat
      in
      let set_dir m = List.rev_map ~f:(fun (cm_kind, p) -> cm_dir m cm_kind, p) in
      let+ modules_impl =
        let+ bin_annot = Env_stanza_db.bin_annot ~dir in
        List.rev_concat_map installable_modules.impl ~f:(fun m ->
          let cmt_files =
            match bin_annot with
            | false -> []
            | true ->
              List.rev_concat_map Ml_kind.all ~f:(fun ml_kind ->
                List.rev_concat_map
                  [ native || byte, Lib_mode.Cm_kind.Ocaml Cmi; melange, Melange Cmi ]
                  ~f:(fun (condition, kind) ->
                    if_
                      condition
                      (kind, Obj_dir.Module.cmt_file obj_dir m ~ml_kind ~cm_kind:kind)))
          in
          List.rev_append (common m) cmt_files |> set_dir m)
      in
      let modules_vlib =
        List.rev_concat_map installable_modules.vlib ~f:(fun m ->
          if Module.kind m = Virtual then [] else common m |> set_dir m)
      in
      modules_vlib @ modules_impl
    and+ lib_files = lib_files ~scope ~dir ~dir_contents ~lib_config info
    and+ execs = lib_ppxs ctx ~scope ~lib
    and+ dll_files =
      dll_files ~modes:ocaml ~dynlink:lib.dynlink ~ctx info
      >>| List.rev_map ~f:(fun a ->
        let entry = Install.Entry.make ~kind:`File Stublibs a in
        Install.Entry.Sourced.create ~loc entry)
    in
    let install_c_headers =
      List.rev_map lib.install_c_headers ~f:(fun (loc, base) ->
        Path.Build.relative dir (base ^ Foreign_language.header_extension)
        |> make_entry ~loc Lib)
    in
    List.rev_concat
      [ sources
      ; melange_runtime_entries
      ; List.rev_map module_files ~f:(fun (sub_dir, file) -> make_entry ?sub_dir Lib file)
      ; (match lib.kind with
         | Parameter -> []
         | Virtual | Dune_file _ ->
           List.rev_concat
             [ List.rev_map lib_files ~f:(fun (section, file) -> make_entry section file)
             ; List.rev_map execs ~f:(make_entry Libexec)
             ; dll_files
             ; install_c_headers
             ; public_headers
             ])
      ]
  ;;

  let keep_if expander ~scope stanza =
    let+ keep =
      match Stanza.repr stanza with
      | Library.T lib ->
        let* enabled_if = Expander.eval_blang expander lib.enabled_if in
        if enabled_if
        then
          if lib.optional
          then (
            let src_dir =
              Expander.dir expander
              |> Path.build
              |> Path.drop_optional_build_context_src_exn
            in
            Lib.DB.available_by_lib_id
              (Scope.libs scope)
              (Local (Library.to_lib_id ~src_dir lib)))
          else Memo.return true
        else Memo.return false
      | Documentation.T _ -> Memo.return true
      | Install_conf.T { enabled_if; _ } -> Expander.eval_blang expander enabled_if
      | Plugin.T _ -> Memo.return true
      | Executables.T ({ install_conf = Some _; _ } as exes) ->
        Expander.eval_blang expander exes.enabled_if
        >>= (function
         | false -> Memo.return false
         | true ->
           if not exes.optional
           then Memo.return true
           else
             let* compile_info =
               let dune_version = Scope.project scope |> Dune_project.dune_version in
               let+ pps =
                 (* This is wrong. If the preprocessors fail to resolve,
                    we shouldn't install the binary rather than failing outright
                 *)
                 Instrumentation.with_instrumentation
                   exes.buildable.preprocess
                   ~instrumentation_backend:
                     (Lib.DB.instrumentation_backend (Scope.libs scope))
                 |> Resolve.Memo.read_memo
                 >>| Preprocess.Per_module.pps
               in
               Lib.DB.resolve_user_written_deps
                 (Scope.libs scope)
                 ~forbidden_libraries:[]
                 (`Exe exes.names)
                 exes.buildable.libraries
                 ~allow_unused_libraries:exes.buildable.allow_unused_libraries
                 ~pps
                 ~dune_version
                 ~allow_overlaps:exes.buildable.allow_overlapping_dependencies
             in
             let+ requires = Lib.Compile.direct_requires compile_info in
             Resolve.is_ok requires)
      | Coq_stanza.Theory.T d -> Memo.return (Option.is_some d.package)
      | Rocq_stanza.Theory.T d -> Memo.return (Option.is_some d.package)
      | _ -> Memo.return false
    in
    Option.some_if keep stanza
  ;;

  let is_odig_doc_file fn =
    List.exists [ "README"; "LICENSE"; "CHANGE"; "HISTORY" ] ~f:(fun prefix ->
      String.is_prefix fn ~prefix)
  ;;

  let entries_of_install_stanza ~dir ~expander ~package_db (install_conf : Install_conf.t)
    =
    let expand = Expander.No_deps.expand expander ~mode:Single in
    let make_entry =
      let section = Package_db.section_of_site package_db in
      fun fb ~kind ->
        let src = File_binding.Expanded.src fb in
        let dst = File_binding.Expanded.dst fb in
        Install_entry_with_site.make_with_site
          ?dst
          ~kind
          (snd install_conf.section)
          section
          src
    in
    let+ files =
      Install_entry.File.to_file_bindings_expanded install_conf.files ~expand ~dir
      >>= Memo.List.map ~f:(fun fb ->
        let+ entry = make_entry ~kind:`File fb in
        let loc = File_binding.Expanded.src_loc fb in
        Install.Entry.Sourced.create ~loc entry)
    and+ files_from_dirs =
      Install_entry.Dir.to_file_bindings_expanded
        install_conf.dirs
        ~expand
        ~dir
        ~relative_dst_path_starts_with_parent_error_when:`Deprecation_warning_from_3_11
      >>= Memo.List.map ~f:(fun fb ->
        let loc = File_binding.Expanded.src_loc fb in
        let+ entry = make_entry ~kind:`Directory fb in
        Install.Entry.Sourced.create ~loc entry)
    and+ source_trees =
      (* There's no deprecation warning when a relative destination path
         starts with a parent in this feature. It's safe to raise an error in
         this case as installing source trees was added in the same dune version
         that we deprecated starting a destination install path with "..". *)
      Install_entry.Dir.to_file_bindings_expanded
        install_conf.source_trees
        ~expand
        ~dir
        ~relative_dst_path_starts_with_parent_error_when:`Always_error
      >>= Memo.List.map ~f:(fun fb ->
        let loc = File_binding.Expanded.src_loc fb in
        let* entry = make_entry ~kind:`Source_tree fb in
        let+ () =
          Source_tree.find_dir (Path.Build.drop_build_context_exn entry.src)
          >>| function
          | Some _ -> ()
          | None ->
            User_error.raise ~loc [ Pp.text "This source directory does not exist" ]
        in
        Install.Entry.Sourced.create ~loc entry)
    in
    List.rev_concat [ files; files_from_dirs; source_trees ]
  ;;

  let stanza_to_entries ~package_db ~sctx ~dir ~scope ~expander stanza =
    (let+ stanza = keep_if expander stanza ~scope in
     let open Option.O in
     let* stanza = stanza in
     let+ package = Stanzas.stanza_package stanza in
     stanza, package)
    >>= function
    | None -> Memo.return None
    | Some (stanza, package) ->
      let+ entries =
        match Stanza.repr stanza with
        | Install_conf.T i | Executables.T { install_conf = Some i; _ } ->
          entries_of_install_stanza ~dir ~expander ~package_db i
        | Library.T lib ->
          let sub_dir = Library.sub_dir lib in
          let* dir_contents = Dir_contents.get sctx ~dir in
          lib_install_files sctx ~scope ~dir ~sub_dir lib ~dir_contents
        | Coq_stanza.Theory.T coqlib -> Coq_rules.install_rules ~sctx ~dir coqlib
        | Rocq_stanza.Theory.T theory -> Rocq_rules.install_rules ~sctx ~dir theory
        | Documentation.T stanza ->
          let* dir_contents = Dir_contents.get sctx ~dir in
          let+ mld_contents = Dir_contents.mlds ~stanza dir_contents in
          doc_install_files ~loc:stanza.loc mld_contents
        | Plugin.T t -> Plugin_rules.install_rules ~sctx ~package_db ~dir t
        | _ -> Memo.return []
      in
      let name = Package.Id.name package in
      Some (name, entries)
  ;;

  module Package_map_traversals = Memo.Make_parallel_map (Package.Name.Map)

  let stanzas_to_entries sctx =
    let ctx = Context.build_context (Super_context.context sctx) in
    let* stanzas = Dune_load.dune_files ctx.name in
    let* packages = Dune_load.packages () in
    let+ init =
      Package_map_traversals.parallel_map packages ~f:(fun _name (pkg : Package.t) ->
        let opam_file = Package_paths.opam_file ctx pkg in
        let init =
          let file section local_file dst =
            Install.Entry.make section local_file ~kind:`File ~dst
            |> Install.Entry.Sourced.create
          in
          let deprecated_meta_and_dune_files =
            Package.deprecated_package_names pkg
            |> Package.Name.Map.to_list
            |> List.rev_concat_map ~f:(fun (name, _) ->
              let meta_file = Package_paths.deprecated_meta_file ctx pkg name in
              let dune_package_file =
                Package_paths.deprecated_dune_package_file ctx pkg name
              in
              let file local_file install_fn =
                file Lib_root local_file (Package.Name.to_string name ^ "/" ^ install_fn)
              in
              [ file meta_file Dune_findlib.Package.meta_fn
              ; file dune_package_file Dune_package.fn
              ])
          in
          let meta_file = Package_paths.meta_file ctx pkg in
          let dune_package_file = Package_paths.dune_package_file ctx pkg in
          let odoc_config_file =
            match Package_paths.odoc_config_file ctx pkg with
            | None -> []
            | Some config_file -> [ file Doc config_file "odoc-config.sexp" ]
          in
          (file Lib meta_file Dune_findlib.Package.meta_fn
           :: file Lib dune_package_file Dune_package.fn
           :: odoc_config_file)
          @
          match opam_file with
          | None -> deprecated_meta_and_dune_files
          | Some opam_file -> file Lib opam_file "opam" :: deprecated_meta_and_dune_files
        in
        let pkg_dir = Package.dir pkg in
        Source_tree.find_dir pkg_dir
        >>| function
        | None -> init
        | Some dir ->
          let pkg_dir = Path.Build.append_source ctx.build_dir pkg_dir in
          Source_tree.Dir.filenames dir
          |> Filename.Set.fold ~init ~f:(fun fn acc ->
            if is_odig_doc_file fn
            then (
              let odig_file = Path.Build.relative pkg_dir fn in
              let entry = Install.Entry.make Doc ~kind:`File odig_file in
              Install.Entry.Sourced.create entry :: acc)
            else acc))
    and+ entries =
      let* package_db = Package_db.create ctx.name in
      Dune_file.fold_static_stanzas stanzas ~init:[] ~f:(fun dune_file stanza acc ->
        let dir = Path.Build.append_source ctx.build_dir (Dune_file.dir dune_file) in
        let named_entries =
          let* expander = Super_context.expander sctx ~dir
          and* scope = Scope.DB.find_by_dir dir in
          stanza_to_entries ~package_db ~sctx ~dir ~scope ~expander stanza
        in
        named_entries :: acc)
      |> Memo.all_concurrently
    in
    List.fold_left entries ~init ~f:(fun acc named_entries ->
      match named_entries with
      | None -> acc
      | Some (name, entries) -> Package.Name.Map.Multi.add_all acc name entries)
    |> Package.Name.Map.map ~f:(fun entries ->
      (* Sort entries so that the ordering in [dune-package] is independent
         of Dune's current implementation. *)
      (* jeremiedimino: later on, we group this list by section and sort
         each section. It feels like we should just do this here once and
         for all. *)
      List.sort
        entries
        ~compare:(fun (a : Install.Entry.Sourced.t) (b : Install.Entry.Sourced.t) ->
          Install.Entry.compare Path.Build.compare a.entry b.entry))
  ;;

  let stanzas_to_entries =
    let memo =
      Memo.create
        ~input:(module Super_context.As_memo_key)
        "stanzas-to-entries"
        stanzas_to_entries
    in
    Memo.exec memo
  ;;
end

module Meta_and_dune_package : sig
  val meta_and_dune_package_rules : Super_context.t -> Dune_project.t -> unit Memo.t
end = struct
  let sections ctx_name files pkg =
    let pkg_name = Package.name pkg in
    let sections =
      (* the one from sites *)
      Package.sites pkg |> Site.Map.values |> Section.Set.of_list
    in
    let sections =
      (* the one from install stanza *)
      List.fold_left ~init:sections files ~f:(fun acc (s, _) -> Section.Set.add acc s)
    in
    Section.Set.to_map sections ~f:(fun section ->
      Install.Paths.get_local_location ctx_name section pkg_name)
  ;;

  let make_dune_package sctx lib_entries (pkg : Package.t) =
    Action_builder.of_memo
    @@
    let pkg_name = Package.name pkg in
    let ctx = Super_context.context sctx in
    let pkg_root =
      Install.Context.lib_dir ~context:(Context.name ctx) ~package:pkg_name
    in
    let lib_root lib =
      let subdir =
        Lib_info.Status.relative_to_package
          (Lib_info.status @@ Lib.info lib)
          (Lib.name lib)
        |> Option.value_exn
      in
      Path.Build.append_local pkg_root subdir
    in
    let* entries =
      let* { Scope.DB.Lib_entry.Set.deprecated_library_names; libraries } = lib_entries in
      let deprecated =
        List.filter_map deprecated_library_names ~f:(function
          | { old_name = _, Deprecated _; _ } -> None
          | { old_name = old_public_name, Not_deprecated
            ; new_public_name = _, new_public_name
            ; loc
            ; project = _
            } ->
            let old_public_name = Public_lib.name old_public_name in
            Some
              ( old_public_name
              , Dune_package.Entry.Deprecated_library_name
                  { loc; old_public_name; new_public_name } ))
      in
      let+ libraries =
        Memo.parallel_map libraries ~f:(fun lib ->
          let info = Lib.Local.info lib in
          let dir = Lib_info.src_dir info in
          let* dir_contents = Dir_contents.get sctx ~dir in
          let obj_dir = Lib.Local.obj_dir lib in
          let lib = Lib.Local.to_lib lib in
          let name = Lib.name lib in
          let* expander = Super_context.expander sctx ~dir in
          let file_deps (deps : _ Lib_info.File_deps.t) =
            match deps with
            | External _paths -> assert false
            | Local (loc, dep_conf) ->
              Lib_file_deps.eval ~expander ~loc ~paths:Allow_all dep_conf
              >>| Path.Set.to_list_map ~f:(fun p ->
                let local_path = p |> Path.as_in_build_dir_exn |> Path.Build.local in
                check_runtime_deps_relative_path ~lib_info:info ~loc local_path;
                p)
          in
          let* foreign_objects =
            (* We are writing the list of .o files to dune-package, but we
               actually only install them for virtual libraries. See
               [Lib_archives.make] *)
            let dir = Obj_dir.obj_dir obj_dir in
            let* ext_obj =
              let+ ocaml = Context.ocaml ctx in
              ocaml.lib_config.ext_obj
            in
            let+ foreign_sources = Dir_contents.foreign_sources dir_contents in
            Foreign_sources.for_lib ~name foreign_sources
            |> Foreign.Sources.object_files ~dir ~ext_obj
            |> List.map ~f:Path.build
          and* modules =
            let* libs = Scope.DB.find_by_dir dir >>| Scope.libs in
            Dir_contents.ocaml dir_contents
            >>= Ml_sources.modules
                  ~libs
                  ~for_:(Library (Lib_info.lib_id info |> Lib_id.to_local_exn))
            >>| Modules.With_vlib.modules
          and* melange_runtime_deps = file_deps (Lib_info.melange_runtime_deps info)
          and* public_headers = file_deps (Lib_info.public_headers info) in
          let+ dune_lib =
            Lib.to_dune_lib
              lib
              ~dir:(Path.build (lib_root lib))
              ~modules
              ~foreign_objects
              ~melange_runtime_deps
              ~public_headers
            >>= Resolve.read_memo
          in
          name, Dune_package.Entry.Library dune_lib)
      in
      List.rev_append libraries deprecated
      |> List.sort ~compare:(fun (x, _) (y, _) -> Lib_name.compare x y)
      |> List.fold_left ~init:Lib_name.Map.empty ~f:(fun acc (name, x) ->
        Lib_name.Map.add_exn acc name x)
    in
    let+ files =
      let+ map = Stanzas_to_entries.stanzas_to_entries sctx in
      Package.Name.Map.Multi.find map pkg_name
      |> List.map ~f:(fun (e : Install.Entry.Sourced.t) ->
        let kind =
          match e.entry.kind with
          | `File -> `File
          | `Directory | `Source_tree -> `Dir
        in
        e.entry.section, (kind, e.entry.dst))
      |> Section.Map.of_list_multi
      |> Section.Map.to_list
    in
    let sections = sections (Context.name ctx) files pkg in
    Dune_package.Or_meta.Dune_package
      { Dune_package.version = Package.version pkg
      ; name = pkg_name
      ; entries
      ; dir = Path.build pkg_root
      ; sections
      ; sites = Package.sites pkg
      ; files
      }
  ;;

  let gen_dune_package sctx (pkg : Package.t) lib_entries =
    let ctx = Super_context.context sctx |> Context.build_context in
    let dune_version = Dune_lang.Syntax.greatest_supported_version_exn Stanza.syntax in
    let action =
      let dune_package_file = Package_paths.dune_package_file ctx pkg in
      Action_builder.write_file_dyn
        dune_package_file
        (let open Action_builder.O in
         Package_paths.meta_template ctx pkg
         |> Path.build
         |> Action_builder.if_file_exists
              ~then_:(Action_builder.return Dune_package.Or_meta.Use_meta)
              ~else_:(make_dune_package sctx lib_entries pkg)
         >>| Format.asprintf
               "%a"
               (Dune_package.Or_meta.pp ~dune_version ~encoding:Relative))
    in
    let* () =
      let deprecated_dune_packages =
        Memo.lazy_ ~name:"deprecated dune packages" (fun () ->
          let+ { deprecated_library_names; _ } = lib_entries in
          List.filter_map deprecated_library_names ~f:(function
            | { Library_redirect.old_name =
                  old_public_name, Deprecated_library_name.Old_name.Deprecated _
              ; _
              } as t -> Some (Lib_name.package_name (Public_lib.name old_public_name), t)
            | _ -> None)
          |> Package.Name.Map.of_list_multi)
        |> Memo.Lazy.force
      in
      Package.deprecated_package_names pkg
      |> Package.Name.Map.to_seq
      |> Memo.parallel_iter_seq ~f:(fun (name, loc) ->
        let dune_pkg =
          let open Action_builder.O in
          let+ dune_pkg =
            let+ deprecated_dune_packages =
              Action_builder.of_memo deprecated_dune_packages
            in
            let entries =
              match Package.Name.Map.find deprecated_dune_packages name with
              | None -> Lib_name.Map.empty
              | Some entries ->
                List.fold_left
                  entries
                  ~init:Lib_name.Map.empty
                  ~f:
                    (fun
                      acc
                      { Library_redirect.old_name = old_public_name, _
                      ; new_public_name = _, new_public_name
                      ; loc
                      ; _
                      }
                    ->
                    let old_public_name = Public_lib.name old_public_name in
                    Lib_name.Map.add_exn
                      acc
                      old_public_name
                      (Dune_package.Entry.Deprecated_library_name
                         { loc; old_public_name; new_public_name }))
            in
            let sections = sections ctx.name [] pkg in
            { Dune_package.version = Package.version pkg
            ; name
            ; entries
            ; dir = Path.build (Install.Context.lib_dir ~context:ctx.name ~package:name)
            ; sections
            ; sites = Package.sites pkg
            ; files = []
            }
          in
          Format.asprintf
            "%a"
            (Dune_package.Or_meta.pp ~dune_version ~encoding:Relative)
            (Dune_package dune_pkg)
        in
        Action_builder.write_file_dyn
          (Package_paths.deprecated_dune_package_file ctx pkg name)
          dune_pkg
        |> Super_context.add_rule sctx ~dir:ctx.build_dir ~loc)
    in
    Super_context.add_rule sctx ~dir:ctx.build_dir action
  ;;

  let gen_odoc_config sctx (pkg : Package.t) =
    let { Dune_lang.Package_documentation.packages } = Package.documentation pkg in
    let ctx = Super_context.context sctx |> Context.build_context in
    match Package_paths.odoc_config_file ctx pkg with
    | None -> Memo.return ()
    | Some odoc_config_file ->
      let action =
        Action_builder.write_file_dyn
          odoc_config_file
          (Action_builder.return
           @@ Format.asprintf
                "(packages %s)"
                (packages
                 |> List.map ~f:(fun (d : Package_dependency.t) ->
                   d.name |> Dune_lang.Package_name.to_string)
                 |> String.concat ~sep:" "))
      in
      Super_context.add_rule sctx ~dir:ctx.build_dir action
  ;;

  let gen_meta_file sctx (pkg : Package.t) entries =
    let ctx = Super_context.context sctx |> Context.build_context in
    let* () =
      let template =
        let meta_template = Path.build (Package_paths.meta_template ctx pkg) in
        let meta_template_lines_or_fail =
          let open Action_builder.O in
          let* () = Action_builder.return () in
          let* { Scope.DB.Lib_entry.Set.libraries; _ } = Action_builder.of_memo entries in
          match
            List.find_map libraries ~f:(fun lib ->
              match Lib_info.kind (Lib.Local.info lib) with
              | Parameter | Virtual -> Some lib
              | Dune_file _ -> None)
          with
          | None -> Action_builder.lines_of meta_template
          | Some vlib ->
            Action_builder.fail
              { fail =
                  (fun () ->
                    let name = Lib.name (Lib.Local.to_lib vlib) in
                    let pkg_name = Package.name pkg in
                    User_error.raise
                      ~loc:(Loc.in_file meta_template)
                      [ Pp.textf
                          "Package %s defines virtual library %s and has a META \
                           template. This is not allowed."
                          (Package.Name.to_string pkg_name)
                          (Lib_name.to_string name)
                      ])
              }
        in
        Action_builder.if_file_exists
          meta_template
          ~then_:meta_template_lines_or_fail
          ~else_:(Action_builder.return [ "# DUNE_GEN" ])
      in
      let meta_entries =
        entries
        >>| Scope.DB.Lib_entry.Set.partition_map ~f:(function
          | Scope.DB.Lib_entry.Deprecated_library_name
              { old_name = public, Deprecated { deprecated_package }; _ } as entry ->
            (match Public_lib.sub_dir public with
             | None -> Left (deprecated_package, entry)
             | Some _ -> Right entry)
          | entry -> Right entry)
        >>| snd
      in
      Super_context.add_rule
        sctx
        ~dir:ctx.build_dir
        (let open Action_builder.O in
         (let+ template = template
          and+ (meta : Meta.t) =
            Action_builder.of_memo meta_entries
            >>= Gen_meta.gen ~package:pkg ~add_directory_entry:true
          in
          let pp =
            Pp.concat_map template ~sep:Pp.newline ~f:(fun s ->
              if String.is_prefix s ~prefix:"#"
              then (
                match String.extract_blank_separated_words (String.drop s 1) with
                | [ ("JBUILDER_GEN" | "DUNE_GEN") ] -> Meta.pp meta.entries
                | _ -> Pp.verbatim s)
              else Pp.verbatim s)
            |> Pp.vbox
          in
          Format.asprintf "%a" Pp.to_fmt pp)
         |> Action_builder.write_file_dyn (Package_paths.meta_file ctx pkg))
    in
    let deprecated_packages =
      Memo.lazy_ ~name:"deprecated packages" (fun () ->
        let+ { Scope.DB.Lib_entry.Set.deprecated_library_names; _ } = entries in
        List.filter_map deprecated_library_names ~f:(function
          | { Library_redirect.old_name =
                public, Deprecated_library_name.Old_name.Deprecated { deprecated_package }
            ; _
            } as entry ->
            (match Public_lib.sub_dir public with
             | None -> Some (deprecated_package, entry)
             | Some _ -> None)
          | _ -> None)
        |> Package.Name.Map.of_list_multi)
      |> Memo.Lazy.force
      |> Action_builder.of_memo
    in
    Package.deprecated_package_names pkg
    |> Dune_lang.Package_name.Map.to_seq
    |> Memo.parallel_iter_seq ~f:(fun (name, loc) ->
      let meta = Package_paths.deprecated_meta_file ctx pkg name in
      Super_context.add_rule
        sctx
        ~dir:ctx.build_dir
        ~loc
        (Action_builder.write_file_dyn
           meta
           (let open Action_builder.O in
            let+ pp =
              let+ meta =
                let* deprecated_packages = deprecated_packages in
                Package.Name.Map.Multi.find deprecated_packages name
                |> List.map ~f:(fun deprecated ->
                  Scope.DB.Lib_entry.Deprecated_library_name deprecated)
                |> Gen_meta.gen ~package:pkg ~add_directory_entry:false
              in
              let open Pp.O in
              Pp.vbox (Meta.pp meta.entries ++ Pp.cut)
            in
            Format.asprintf "%a" Pp.to_fmt pp)))
  ;;

  let meta_and_dune_package_rules sctx project =
    let ctx = Super_context.context sctx |> Context.name in
    Dune_project.packages project
    |> Dune_lang.Package_name.Map.to_seq
    |> Memo.parallel_iter_seq ~f:(fun (name, (pkg : Package.t)) ->
      let entries = Scope.DB.lib_entries_of_package ctx name in
      gen_dune_package sctx pkg entries
      >>> gen_meta_file sctx pkg entries
      >>> gen_odoc_config sctx pkg)
  ;;
end

include Meta_and_dune_package

let symlink_source_dir ~dir ~dst =
  let+ _, files = Source_deps.files dir in
  Path.Set.to_list_map files ~f:(fun src ->
    let suffix = Path.drop_prefix_exn ~prefix:dir src in
    let dst = Path.Build.append_local dst suffix in
    suffix, dst, Action_builder.symlink ~src ~dst)
;;

let symlink_installed_artifacts_to_build_install
      (ctx : Build_context.t)
      (entries : Install.Entry.Sourced.t list)
      ~install_paths
  =
  let install_dir = Install.Context.dir ~context:ctx.name in
  Memo.parallel_map entries ~f:(fun (s : Install.Entry.Sourced.t) ->
    let entry = s.entry in
    let dst =
      let relative =
        Install.Entry.relative_installed_path entry ~paths:install_paths
        |> Path.as_in_source_tree_exn
      in
      Path.Build.append_source install_dir relative
    in
    let loc =
      match s.source with
      | User l -> l
      | Dune -> Loc.in_file (Path.build entry.src)
    in
    let src = Path.build entry.src in
    let rule { Action_builder.With_targets.targets; build } =
      Rule.make ~info:(From_dune_file loc) ~targets build
    in
    match entry.kind with
    | `Source_tree ->
      symlink_source_dir ~dir:src ~dst
      >>| List.map ~f:(fun (suffix, dst, build) ->
        let rule = rule build in
        let entry =
          let entry =
            Install.Entry.map_dst entry ~f:(fun dst ->
              Install.Entry.Dst.add_suffix dst (Path.Local.to_string suffix))
          in
          let entry = Install.Entry.set_src entry dst in
          Install.Entry.set_kind entry `File
        in
        { s with entry }, rule)
    | (`File | `Directory) as kind ->
      let entry =
        let entry = Install.Entry.set_src entry dst in
        { s with entry }
      in
      let action =
        (match kind with
         | `File -> Action_builder.symlink
         | `Directory -> Action_builder.symlink_dir)
          ~src
          ~dst
      in
      Memo.return [ entry, rule action ])
;;

let promote_install_file (ctx : Context.t) =
  !Clflags.promote_install_files
  && (not (Context.implicit ctx))
  &&
  match Context.kind ctx with
  | Lock _ | Default -> true
  | Opam _ -> false
;;

let install_entries sctx package =
  let+ packages = Stanzas_to_entries.stanzas_to_entries sctx in
  Package.Name.Map.Multi.find packages package
;;

let packages =
  let f sctx =
    let* packages = Dune_load.packages () in
    let packages = Package.Name.Map.values packages in
    let+ l =
      Memo.parallel_map packages ~f:(fun (pkg : Package.t) ->
        Package.name pkg
        |> install_entries sctx
        >>| List.map ~f:(fun (e : Install.Entry.Sourced.t) -> e.entry.src, Package.id pkg))
    in
    List.rev_concat l
    |> Path.Build.Map.of_list_fold ~init:Package.Id.Set.empty ~f:Package.Id.Set.add
  in
  let memo =
    Memo.create
      "package-map"
      f
      ~input:(module Super_context.As_memo_key)
      ~cutoff:(Path.Build.Map.equal ~equal:Package.Id.Set.equal)
  in
  fun sctx -> Memo.exec memo sctx
;;

let packages_file_is_part_of path =
  Memo.Option.bind
    (let open Option.O in
     let* ctx_name, _ = Path.Build.extract_build_context path in
     Context_name.of_string_opt ctx_name)
    ~f:Super_context.find
  >>= function
  | None -> Memo.return Package.Id.Set.empty
  | Some sctx ->
    let open Memo.O in
    let+ map = packages sctx in
    Option.value (Path.Build.Map.find map path) ~default:Package.Id.Set.empty
;;

let symlinked_entries sctx package =
  let install_paths =
    let roots = Install.Roots.opam_from_prefix Path.root ~relative:Path.relative in
    Install.Paths.make ~relative:Path.relative ~package ~roots
  in
  let build_context = Super_context.context sctx |> Context.build_context in
  install_entries sctx package
  >>= symlink_installed_artifacts_to_build_install build_context ~install_paths
  >>| List.rev_concat
  >>| List.split
;;

let symlinked_entries =
  let memo =
    Memo.create
      ~input:(module Super_context.As_memo_key.And_package_name)
      ~human_readable_description:(fun (_, pkg) ->
        Pp.textf
          "Computing installable artifacts for package %s"
          (Package.Name.to_string pkg))
      "symlinked_entries"
      (fun (sctx, pkg) -> symlinked_entries sctx pkg)
  in
  fun sctx pkg -> Memo.exec memo (sctx, pkg)
;;

let package_deps (pkg : Package.t) files =
  let rec loop rules_seen (fn : Path.Build.t) =
    let* pkgs = packages_file_is_part_of fn in
    if Package.Id.Set.is_empty pkgs || Package.Id.Set.mem pkgs (Package.id pkg)
    then loop_deps rules_seen fn
    else Memo.return (pkgs, rules_seen)
  and loop_deps rules_seen fn =
    Load_rules.get_rule (Path.build fn)
    >>= function
    | None -> Memo.return (Package.Id.Set.empty, rules_seen)
    | Some rule ->
      if Rule.Set.mem rules_seen rule
      then Memo.return (Package.Id.Set.empty, rules_seen)
      else (
        let rules_seen = Rule.Set.add rules_seen rule in
        let* res = Dune_engine.Build_system.execute_rule rule in
        loop_files
          rules_seen
          (Dep.Facts.paths ~expand_aliases:true res.facts
           |> Path.Set.to_list
           |>
           (* if this file isn't in the build dir, it doesn't belong to any
                 package and it doesn't have dependencies that do *)
           List.filter_map ~f:Path.as_in_build_dir))
  and loop_files rules_seen files =
    Memo.List.fold_left
      ~init:(Package.Id.Set.empty, rules_seen)
      files
      ~f:(fun (sets, rules_seen) file ->
        let+ set, rules_seen = loop rules_seen file in
        Package.Id.Set.union set sets, rules_seen)
  in
  let+ packages, _rules_seen = loop_files Rule.Set.empty files in
  packages
;;

include (
struct
  module Spec = struct
    type ('path, 'target) t = Path.t Install.Entry.t list * 'target

    let name = "gen-install-file"
    let version = 2
    let bimap (entries, dst) _ g = entries, g dst
    let is_useful_to ~memoize = memoize
    let encode (_entries, dst) _path target : Sexp.t = List [ target dst ]

    let make_entry entry path comps =
      Install.Entry.set_src entry path
      |> Install.Entry.map_dst ~f:(fun dst -> Install.Entry.Dst.concat_all dst comps)
    ;;

    let read_dir_recursively (entry : _ Install.Entry.t) =
      let rec loop acc dirs =
        match dirs with
        | [] ->
          List.rev_map acc ~f:(fun (path, comps) ->
            let comps = List.rev comps in
            make_entry entry path comps)
          |> List.sort ~compare:(fun (x : _ Install.Entry.t) (y : _ Install.Entry.t) ->
            Path.compare x.src y.src)
        | (dir, comps) :: dirs ->
          (match Path.Untracked.readdir_unsorted_with_kinds dir with
           | Error (e, x, y) -> raise (Unix.Unix_error (e, x, y))
           | Ok files ->
             let files, new_dirs =
               List.partition_map files ~f:(fun (name, kind) ->
                 let path = Path.relative dir name in
                 let comps = name :: comps in
                 match kind with
                 | Unix.S_DIR -> Right (path, comps)
                 | _ -> Left (path, comps))
             in
             let acc = List.rev_append files acc in
             let dirs = List.rev_append new_dirs dirs in
             loop acc dirs)
      in
      loop [] [ entry.src, [] ]
    ;;

    let action (entries, dst) ~ectx:_ ~eenv:_ =
      let open Fiber.O in
      let* entries =
        let+ entries =
          Fiber.parallel_map entries ~f:(fun (entry : _ Install.Entry.t) ->
            match entry.kind with
            | `File -> Fiber.return [ entry ]
            | `Directory -> Fiber.return (read_dir_recursively entry)
            | `Source_tree ->
              Code_error.raise
                "This entry should have been expanded into `File"
                [ "entry", Install.Entry.to_dyn Path.to_dyn entry ])
        in
        List.concat entries |> Install.Entry.gen_install_file
      in
      Async.async (fun () -> Io.write_file (Path.build dst) entries)
    ;;
  end

  module A = Action_ext.Make (Spec)

  let gen_install_file entries ~dst = A.action (entries, dst)
end :
sig
  val gen_install_file : Path.t Install.Entry.t list -> dst:Path.Build.t -> Action.t
end)

let gen_package_install_file_rules sctx (package : Package.t) =
  let package_name = Package.name package in
  let roots = Install.Roots.opam_from_prefix Path.root ~relative:Path.relative in
  let install_paths =
    Install.Paths.make ~relative:Path.relative ~package:package_name ~roots
  in
  let entries = Action_builder.of_memo (symlinked_entries sctx package_name >>| fst) in
  let context = Super_context.context sctx in
  let build_context = Context.build_context context in
  let pkg_build_dir = Package_paths.build_dir build_context package in
  let files =
    Action_builder.map
      entries
      ~f:(List.rev_map ~f:(fun (e : Install.Entry.Sourced.t) -> e.entry.src))
    |> Action_builder.memoize "entries"
  in
  let* dune_project = Dune_load.find_project ~dir:pkg_build_dir in
  let strict_package_deps = Dune_project.strict_package_deps dune_project in
  let packages =
    let open Action_builder.O in
    let+ packages =
      let* files = files in
      Action_builder.of_memo (package_deps package files)
    in
    (match strict_package_deps with
     | false -> ()
     | true ->
       let missing_deps =
         let effective_deps =
           Package.Id.Set.to_list packages
           |> Package.Name.Set.of_list_map ~f:Package.Id.name
         in
         let specified_deps =
           Package.depends package
           |> Package.Name.Set.of_list_map ~f:(fun (dep : Package_dependency.t) ->
             dep.name)
         in
         Package.Name.Set.diff effective_deps specified_deps
       in
       if not (Package.Name.Set.is_empty missing_deps)
       then (
         let name = Package.name package in
         User_error.raise
           [ Pp.textf
               "Package %s is missing the following package dependencies"
               (Package.Name.to_string name)
           ; Package.Name.Set.to_list missing_deps
             |> Pp.enumerate ~f:(fun name -> Pp.text (Package.Name.to_string name))
           ]));
    packages
  in
  let install_file_deps =
    let open Action_builder.O in
    files >>| Path.Set.of_list_map ~f:Path.build >>= Action_builder.path_set
  in
  let* () =
    let* all_packages = Dune_load.packages () in
    let target_alias =
      Dep_conf_eval.package_install ~context:build_context ~pkg:package
    in
    let open Action_builder.O in
    Rules.Produce.Alias.add_deps
      target_alias
      (Action_builder.dyn_deps
         (let+ packages = packages
          and+ () = install_file_deps in
          ( ()
          , Package.Id.Set.to_list packages
            |> Dep.Set.of_list_map ~f:(fun (pkg : Package.Id.t) ->
              let pkg =
                let name = Package.Id.name pkg in
                Package.Name.Map.find_exn all_packages name
              in
              Dep_conf_eval.package_install ~context:build_context ~pkg |> Dep.alias) )))
  in
  let action =
    let findlib_toolchain = Context.findlib_toolchain context in
    let install_file =
      Path.Build.relative
        pkg_build_dir
        (install_file ~package:package_name ~findlib_toolchain)
    in
    let open Action_builder.O in
    let entries =
      let+ () = install_file_deps
      and+ () =
        if strict_package_deps
        then Action_builder.map packages ~f:(fun (_ : Package.Id.Set.t) -> ())
        else Action_builder.return ()
      and+ entries =
        let+ entries = entries in
        match findlib_toolchain with
        | None -> entries
        | Some toolchain ->
          let prefix =
            let toolchain = Context_name.to_string toolchain in
            Path.of_string (toolchain ^ "-sysroot")
          in
          List.rev_map entries ~f:(fun (e : Install.Entry.Sourced.t) ->
            { e with
              entry =
                Install.Entry.add_install_prefix e.entry ~paths:install_paths ~prefix
            })
      in
      if not (Package.allow_empty package)
      then
        if
          List.for_all entries ~f:(fun (e : Install.Entry.Sourced.t) ->
            match e.source with
            | Dune -> true
            | User _ -> false)
        then (
          let is_error = Dune_project.dune_version dune_project >= (3, 0) in
          User_warning.emit
            ~is_error
            [ Pp.textf
                "The package %s does not have any user defined stanzas attached to it. \
                 If this is intentional, add (allow_empty) to the package definition in \
                 the dune-project file"
                (Package.Name.to_string package_name)
            ]);
      List.rev_map entries ~f:(fun (e : Install.Entry.Sourced.t) ->
        Install.Entry.set_src e.entry (Path.build e.entry.src))
    in
    entries
    >>| gen_install_file ~dst:install_file
    >>| Action.Full.make
    |> Action_builder.with_file_targets ~file_targets:[ install_file ]
  in
  Super_context.add_rule
    sctx
    ~dir:pkg_build_dir
    ~mode:
      (if promote_install_file context
       then Promote { lifetime = Until_clean; into = None; only = None }
       else
         (* We must ignore the source file since it might be copied to the source
            tree by another context. *)
         Ignore_source_files)
    action
;;

let memo =
  Memo.create
    ~input:(module Super_context.As_memo_key.And_package_name)
    ~human_readable_description:(fun (_, pkg) ->
      Pp.textf
        "Computing installable artifacts for package %s"
        (Package.Name.to_string pkg))
    "install-rules-and-pkg-entries"
    (fun (sctx, pkg) ->
       Memo.return
         (Scheme.Approximation
            ( (let ctx = Super_context.context sctx in
               Dir_set.subtree (Install.Context.dir ~context:(Context.name ctx)))
            , Thunk
                (fun () ->
                  let+ rules =
                    symlinked_entries sctx pkg >>| snd >>| Rules.of_rules >>| Rules.to_map
                  in
                  Scheme.Finite rules) )))
;;

let scheme sctx pkg = Memo.exec memo (sctx, pkg)

let scheme_per_ctx_memo =
  Memo.create
    ~input:(module Super_context.As_memo_key)
    "install-rule-scheme"
    (fun sctx ->
       Dune_load.packages ()
       >>| Package.Name.Map.values
       >>= Memo.parallel_map ~f:(fun pkg -> scheme sctx (Package.name pkg))
       >>| Scheme.all
       >>= Scheme.evaluate ~union:Rules.Dir_rules.union)
;;

let symlink_rules sctx ~dir =
  let+ rules, subdirs =
    Memo.exec scheme_per_ctx_memo sctx >>= Scheme.Evaluated.get_rules ~dir
  in
  ( Subdir_set.of_set subdirs
  , match rules with
    | None -> Rules.empty
    | Some rules -> Rules.of_dir_rules ~dir rules )
;;

let gen_install_alias sctx (package : Package.t) =
  let context = Super_context.context sctx in
  let name = Package.name package in
  if Context.implicit context
  then Memo.return ()
  else (
    let install_fn =
      install_file ~package:name ~findlib_toolchain:(Context.findlib_toolchain context)
    in
    let path = Package_paths.build_dir (Context.build_context context) package in
    let install_alias = Alias.make Alias0.install ~dir:path in
    let install_file = Path.relative (Path.build path) install_fn in
    Rules.Produce.Alias.add_deps install_alias (Action_builder.path install_file))
;;

let stanzas_to_entries = Stanzas_to_entries.stanzas_to_entries

let gen_project_rules sctx project =
  let* () = meta_and_dune_package_rules sctx project in
  Dune_project.packages project
  |> Dune_lang.Package_name.Map.to_seq
  |> Memo.parallel_iter_seq ~f:(fun (_name, package) ->
    let* () = gen_package_install_file_rules sctx package in
    gen_install_alias sctx package)
;;
