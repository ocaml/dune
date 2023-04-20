open Import
open Memo.O
module Library = Dune_file.Library

let install_file ~(package : Package.Name.t) ~findlib_toolchain =
  let package = Package.Name.to_string package in
  match findlib_toolchain with
  | None -> package ^ ".install"
  | Some x -> sprintf "%s-%s.install" package (Context_name.to_string x)

module Meta_and_dune_package : sig
  val meta_and_dune_package_rules :
    Super_context.t -> Dune_project.t -> unit Memo.t
end = struct
  let sections ctx_name files pkg =
    let pkg_name = Package.name pkg in
    let sections =
      (* the one from sites *)
      Section.Site.Map.values pkg.sites |> Section.Set.of_list
    in
    let sections =
      (* the one from install stanza *)
      List.fold_left ~init:sections files ~f:(fun acc (s, _) ->
          Section.Set.add acc s)
    in
    Section.Set.to_map sections ~f:(fun section ->
        Install.Section.Paths.get_local_location ctx_name section pkg_name)

  (* TODO delay the library resolution errors here. We should still be load
     the [dune-package] file rule even if some libraries are missing *)
  let make_dune_package sctx lib_entries (pkg : Package.t) =
    let pkg_name = Package.name pkg in
    let ctx = Super_context.context sctx in
    let pkg_root =
      Local_install_path.lib_dir ~context:ctx.name ~package:pkg_name
    in
    let lib_root lib =
      let subdir =
        let name = Lib.name lib in
        let _, subdir = Lib_name.split name in
        match
          let info = Lib.info lib in
          Lib_info.status info
        with
        | Private (_, Some _) ->
          Lib_name.Local.mangled_path_under_package (Lib_name.to_local_exn name)
          @ subdir
        | _ -> subdir
      in
      Path.Build.L.relative pkg_root subdir
    in
    let* entries =
      Memo.parallel_map lib_entries ~f:(fun (stanza : Scope.DB.Lib_entry.t) ->
          match stanza with
          | Deprecated_library_name { old_name = _, Deprecated _; _ } ->
            Memo.return None
          | Deprecated_library_name
              { old_name = old_public_name, Not_deprecated
              ; new_public_name = _, new_public_name
              ; loc
              ; project = _
              } ->
            let old_public_name = Dune_file.Public_lib.name old_public_name in
            Memo.return
              (Some
                 ( old_public_name
                 , Dune_package.Entry.Deprecated_library_name
                     { loc; old_public_name; new_public_name } ))
          | Library lib ->
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
                >>| Path.Set.to_list
            in
            let* foreign_objects =
              (* We are writing the list of .o files to dune-package, but we
                 actually only install them for virtual libraries. See
                 [Lib_archives.make] *)
              let dir = Obj_dir.obj_dir obj_dir in
              let+ foreign_sources =
                Dir_contents.foreign_sources dir_contents
              in
              Foreign_sources.for_lib ~name foreign_sources
              |> Foreign.Sources.object_files ~dir
                   ~ext_obj:ctx.lib_config.ext_obj
              |> List.map ~f:Path.build
            and* modules =
              Dir_contents.ocaml dir_contents
              >>| Ml_sources.modules ~for_:(Library name)
            and* melange_runtime_deps =
              file_deps (Lib_info.melange_runtime_deps info)
            and* public_headers = file_deps (Lib_info.public_headers info) in
            let+ sub_systems =
              Lib.to_dune_lib lib
                ~dir:(Path.build (lib_root lib))
                ~modules ~foreign_objects ~melange_runtime_deps ~public_headers
              >>= Resolve.read_memo
            in
            Some (name, Dune_package.Entry.Library sub_systems))
    in
    let entries =
      List.fold_left entries ~init:Lib_name.Map.empty ~f:(fun acc x ->
          match x with
          | None -> acc
          | Some (name, x) -> Lib_name.Map.add_exn acc name x)
    in
    let+ files =
      let+ map = Stanzas_to_entries.stanzas_to_entries sctx in
      Package.Name.Map.Multi.find map pkg_name
      |> List.map ~f:(fun (e : Install.Entry.Sourced.t) ->
             (e.entry.section, e.entry.dst))
      |> Section.Map.of_list_multi |> Section.Map.to_list
    in
    let sections = sections ctx.name files pkg in
    Dune_package.Or_meta.Dune_package
      { Dune_package.version = pkg.version
      ; name = pkg_name
      ; entries
      ; dir = Path.build pkg_root
      ; sections
      ; sites = pkg.sites
      ; files
      }

  let gen_dune_package sctx (pkg : Package.t) =
    let ctx = Super_context.context sctx in
    let dune_version =
      Dune_lang.Syntax.greatest_supported_version Stanza.syntax
    in
    let* lib_entries = Scope.DB.lib_entries_of_package ctx (Package.name pkg) in
    let action =
      let dune_package_file = Package_paths.dune_package_file ctx pkg in
      let meta_template = Package_paths.meta_template ctx pkg in
      Action_builder.write_file_dyn dune_package_file
        (let open Action_builder.O in
        let+ pkg =
          Action_builder.if_file_exists (Path.build meta_template)
            ~then_:(Action_builder.return Dune_package.Or_meta.Use_meta)
            ~else_:
              (Action_builder.of_memo
                 (Memo.bind (Memo.return ()) ~f:(fun () ->
                      make_dune_package sctx lib_entries pkg)))
        in
        Format.asprintf "%a" (Dune_package.Or_meta.pp ~dune_version) pkg)
    in
    let deprecated_dune_packages =
      List.filter_map lib_entries ~f:(function
        | Deprecated_library_name
            ({ old_name = old_public_name, Deprecated _; _ } as t) ->
          Some
            ( Lib_name.package_name (Dune_file.Public_lib.name old_public_name)
            , t )
        | _ -> None)
      |> Package.Name.Map.of_list_multi
    in
    let* () =
      Package.Name.Map.foldi pkg.deprecated_package_names ~init:(Memo.return ())
        ~f:(fun name loc acc ->
          acc
          >>>
          let dune_pkg =
            let entries =
              match Package.Name.Map.find deprecated_dune_packages name with
              | None -> Lib_name.Map.empty
              | Some entries ->
                List.fold_left entries ~init:Lib_name.Map.empty
                  ~f:(fun
                       acc
                       { Dune_file.Library_redirect.old_name =
                           old_public_name, _
                       ; new_public_name = _, new_public_name
                       ; loc
                       ; _
                       }
                     ->
                    let old_public_name =
                      Dune_file.Public_lib.name old_public_name
                    in
                    Lib_name.Map.add_exn acc old_public_name
                      (Dune_package.Entry.Deprecated_library_name
                         { loc; old_public_name; new_public_name }))
            in
            let sections = sections ctx.name [] pkg in
            { Dune_package.version = pkg.version
            ; name
            ; entries
            ; dir =
                Path.build
                  (Local_install_path.lib_dir ~context:ctx.name ~package:name)
            ; sections
            ; sites = pkg.sites
            ; files = []
            }
          in
          let action_with_targets =
            Action_builder.write_file
              (Package_paths.deprecated_dune_package_file ctx pkg
                 dune_pkg.Dune_package.name)
              (Format.asprintf "%a"
                 (Dune_package.Or_meta.pp ~dune_version)
                 (Dune_package.Or_meta.Dune_package dune_pkg))
          in
          Super_context.add_rule sctx ~dir:ctx.build_dir ~loc
            action_with_targets)
    in
    Super_context.add_rule sctx ~dir:ctx.build_dir action

  let gen_meta_file sctx (pkg : Package.t) =
    let ctx = Super_context.context sctx in
    let pkg_name = Package.name pkg in
    let* deprecated_packages, entries =
      let+ entries = Scope.DB.lib_entries_of_package ctx pkg_name in
      List.partition_map entries ~f:(function
        | Deprecated_library_name
            { old_name = public, Deprecated { deprecated_package }; _ } as entry
          -> (
          match Dune_file.Public_lib.sub_dir public with
          | None -> Left (deprecated_package, entry)
          | Some _ -> Right entry)
        | entry -> Right entry)
    in
    let template =
      let meta_template = Path.build (Package_paths.meta_template ctx pkg) in
      let meta_template_lines_or_fail =
        (* XXX this should really be lazy as it's only necessary for the then
           clause. There's no way to express this in the action builder
           however. *)
        let vlib =
          List.find_map entries ~f:(function
            | Library lib ->
              let info = Lib.Local.info lib in
              Option.some_if (Option.is_some (Lib_info.virtual_ info)) lib
            | Deprecated_library_name _ -> None)
        in
        match vlib with
        | None -> Action_builder.lines_of meta_template
        | Some vlib ->
          Action_builder.fail
            { fail =
                (fun () ->
                  let name = Lib.name (Lib.Local.to_lib vlib) in
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
      Action_builder.if_file_exists meta_template
        ~then_:meta_template_lines_or_fail
        ~else_:(Action_builder.return [ "# DUNE_GEN" ])
    in
    let ctx = Super_context.context sctx in
    let meta = Package_paths.meta_file ctx pkg in
    let* () =
      Super_context.add_rule sctx ~dir:ctx.build_dir
        (let open Action_builder.O in
        (let* template = template in
         let+ meta =
           Action_builder.of_memo
             (Gen_meta.gen ~package:pkg ~add_directory_entry:true entries)
         in
         let pp =
           Pp.vbox
             (Pp.concat_map template ~sep:Pp.newline ~f:(fun s ->
                  if String.is_prefix s ~prefix:"#" then
                    match
                      String.extract_blank_separated_words (String.drop s 1)
                    with
                    | [ ("JBUILDER_GEN" | "DUNE_GEN") ] -> Meta.pp meta.entries
                    | _ -> Pp.verbatim s
                  else Pp.verbatim s))
         in
         Format.asprintf "%a" Pp.to_fmt pp)
        |> Action_builder.write_file_dyn meta)
    in
    let deprecated_packages =
      Package.Name.Map.of_list_multi deprecated_packages
    in
    Package.Name.Map_traversals.parallel_iter pkg.deprecated_package_names
      ~f:(fun name loc ->
        let meta = Package_paths.deprecated_meta_file ctx pkg name in
        Super_context.add_rule sctx ~dir:ctx.build_dir ~loc
          (Action_builder.write_file_dyn meta
             (let open Action_builder.O in
             let+ meta =
               let entries =
                 match Package.Name.Map.find deprecated_packages name with
                 | None -> []
                 | Some entries -> entries
               in
               Action_builder.of_memo
                 (Gen_meta.gen ~package:pkg entries ~add_directory_entry:false)
             in
             let pp =
               let open Pp.O in
               Pp.vbox (Meta.pp meta.entries ++ Pp.cut)
             in
             Format.asprintf "%a" Pp.to_fmt pp)))

  let meta_and_dune_package_rules sctx project =
    Dune_project.packages project
    |> Package.Name.Map_traversals.parallel_iter
         ~f:(fun _name (pkg : Package.t) ->
           gen_dune_package sctx pkg >>> gen_meta_file sctx pkg)
end

include Meta_and_dune_package

let symlink_installed_artifacts_to_build_install sctx
    (entries : Install.Entry.Sourced.t list) ~install_paths =
  let ctx = Super_context.context sctx |> Context.build_context in
  let install_dir = Local_install_path.dir ~context:ctx.name in
  List.map entries ~f:(fun (s : Install.Entry.Sourced.t) ->
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
      let rule =
        let { Action_builder.With_targets.targets; build } =
          (match entry.kind with
          | `File -> Action_builder.symlink
          | `Directory -> Action_builder.symlink_dir)
            ~src:(Path.build entry.src) ~dst
        in
        Rule.make
          ~info:(Rule.Info.of_loc_opt (Some loc))
          ~context:(Some ctx) ~targets build
      in
      ({ s with entry = Install.Entry.set_src entry dst }, rule))

let promote_install_file (ctx : Context.t) =
  !Clflags.promote_install_files
  && (not ctx.implicit)
  &&
  match ctx.kind with
  | Default -> true
  | Opam _ -> false

let install_entries sctx (package : Package.t) =
  let+ packages = Stanzas_to_entries.stanzas_to_entries sctx in
  Package.Name.Map.Multi.find packages (Package.name package)

let packages =
  let f sctx =
    let* packages = Only_packages.get () in
    let packages = Package.Name.Map.values packages in
    let+ l =
      Memo.parallel_map packages ~f:(fun (pkg : Package.t) ->
          install_entries sctx pkg
          >>| List.map ~f:(fun (e : Install.Entry.Sourced.t) ->
                  (e.entry.src, pkg.id)))
    in
    Path.Build.Map.of_list_fold (List.concat l) ~init:Package.Id.Set.empty
      ~f:Package.Id.Set.add
  in
  let memo =
    Memo.create "package-map" f
      ~input:(module Super_context.As_memo_key)
      ~cutoff:(Path.Build.Map.equal ~equal:Package.Id.Set.equal)
  in
  fun sctx -> Memo.exec memo sctx

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

let symlinked_entries sctx package =
  let package_name = Package.name package in
  let roots = Install.Section.Paths.Roots.opam_from_prefix Path.root in
  let install_paths = Install.Section.Paths.make ~package:package_name ~roots in
  let+ entries = install_entries sctx package in
  symlink_installed_artifacts_to_build_install sctx ~install_paths entries
  |> List.split

let symlinked_entries =
  let memo =
    Memo.create
      ~input:(module Super_context.As_memo_key.And_package)
      ~human_readable_description:(fun (_, pkg) ->
        Pp.textf "Computing installable artifacts for package %s"
          (Package.Name.to_string (Package.name pkg)))
      "symlinked_entries"
      (fun (sctx, pkg) -> symlinked_entries sctx pkg)
  in
  fun sctx pkg -> Memo.exec memo (sctx, pkg)

let package_deps (pkg : Package.t) files =
  let rec loop rules_seen (fn : Path.Build.t) =
    let* pkgs = packages_file_is_part_of fn in
    if Package.Id.Set.is_empty pkgs || Package.Id.Set.mem pkgs pkg.id then
      loop_deps rules_seen fn
    else Memo.return (pkgs, rules_seen)
  and loop_deps rules_seen fn =
    Load_rules.get_rule (Path.build fn) >>= function
    | None -> Memo.return (Package.Id.Set.empty, rules_seen)
    | Some rule ->
      if Rule.Set.mem rules_seen rule then
        Memo.return (Package.Id.Set.empty, rules_seen)
      else
        let rules_seen = Rule.Set.add rules_seen rule in
        let* res = Dune_engine.Build_system.execute_rule rule in
        loop_files rules_seen
          (Dep.Facts.paths res.deps |> Path.Map.keys
          |> (* if this file isn't in the build dir, it doesn't belong to any
                package and it doesn't have dependencies that do *)
          List.filter_map ~f:Path.as_in_build_dir)
  and loop_files rules_seen files =
    Memo.List.fold_left ~init:(Package.Id.Set.empty, rules_seen) files
      ~f:(fun (sets, rules_seen) file ->
        let+ set, rules_seen = loop rules_seen file in
        (Package.Id.Set.union set sets, rules_seen))
  in
  let+ packages, _rules_seen = loop_files Rule.Set.empty files in
  packages

include (
  struct
    module Spec = struct
      type ('path, 'target) t = Path.t Install.Entry.t list * 'target

      let name = "gen-install-file"

      let version = 1

      let bimap (entries, dst) _ g = (entries, g dst)

      let is_useful_to ~distribute:_ ~memoize = memoize

      let encode (_entries, dst) _path target : Dune_lang.t =
        List [ Dune_lang.atom_or_quoted_string name; target dst ]

      let read_dir_recursively (entry : _ Install.Entry.t) =
        let rec loop acc dirs =
          match dirs with
          | [] ->
            List.rev_map acc ~f:(fun (path, comps) ->
                let comps = List.rev comps in
                Install.Entry.set_src entry path
                |> Install.Entry.map_dst ~f:(fun dst ->
                       Install.Dst.concat_all dst comps))
            |> List.sort
                 ~compare:(fun (x : _ Install.Entry.t) (y : _ Install.Entry.t)
                          -> Path.compare x.src y.src)
          | (dir, comps) :: dirs -> (
            match Path.Untracked.readdir_unsorted_with_kinds dir with
            | Error _ -> Code_error.raise "unable to read directory" []
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
        loop [] [ (entry.src, []) ]

      let action (entries, dst) ~ectx:_ ~eenv:_ =
        let entries =
          List.concat_map entries ~f:(fun (entry : _ Install.Entry.t) ->
              match entry.kind with
              | `File -> [ entry ]
              | `Directory -> read_dir_recursively entry)
          |> Install.gen_install_file
        in
        Io.write_file (Path.build dst) entries;
        Fiber.return ()
    end

    let gen_install_file entries ~dst =
      let module M = struct
        type path = Path.t

        type target = Path.Build.t

        module Spec = Spec

        let v = (entries, dst)
      end in
      Dune_engine.Action.Extension (module M)
  end :
    sig
      val gen_install_file :
        Path.t Install.Entry.t list -> dst:Path.Build.t -> Action.t
    end)

let gen_package_install_file_rules sctx (package : Package.t) =
  let package_name = Package.name package in
  let roots = Install.Section.Paths.Roots.opam_from_prefix Path.root in
  let install_paths = Install.Section.Paths.make ~package:package_name ~roots in
  let entries =
    Action_builder.of_memo (symlinked_entries sctx package >>| fst)
  in
  let ctx = Super_context.context sctx in
  let pkg_build_dir = Package_paths.build_dir ctx package in
  let files =
    Action_builder.map entries
      ~f:(List.map ~f:(fun (e : Install.Entry.Sourced.t) -> e.entry.src))
  in
  let* dune_project =
    let+ scope = Scope.DB.find_by_dir pkg_build_dir in
    Scope.project scope
  in
  let strict_package_deps = Dune_project.strict_package_deps dune_project in
  let packages =
    let open Action_builder.O in
    let* files = files in
    let+ packages = Action_builder.of_memo (package_deps package files) in
    (match strict_package_deps with
    | false -> ()
    | true ->
      let missing_deps =
        let effective_deps =
          Package.Id.Set.to_list packages
          |> Package.Name.Set.of_list_map ~f:Package.Id.name
        in
        Package.missing_deps package ~effective_deps
      in
      if not (Package.Name.Set.is_empty missing_deps) then
        let name = Package.name package in
        User_error.raise
          [ Pp.textf "Package %s is missing the following package dependencies"
              (Package.Name.to_string name)
          ; Package.Name.Set.to_list missing_deps
            |> Pp.enumerate ~f:(fun name ->
                   Pp.text (Package.Name.to_string name))
          ]);
    packages
  in
  let install_file_deps =
    let open Action_builder.O in
    let* files = files in
    Path.Set.of_list_map files ~f:Path.build |> Action_builder.path_set
  in
  let* () =
    let* all_packages = Only_packages.get () in
    let context = Context.build_context ctx in
    let target_alias = Dep_conf_eval.package_install ~context ~pkg:package in
    let open Action_builder.O in
    Rules.Produce.Alias.add_deps target_alias
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
                   Dep_conf_eval.package_install ~context ~pkg |> Dep.alias) )))
  in
  let action =
    let install_file =
      Path.Build.relative pkg_build_dir
        (install_file ~package:package_name
           ~findlib_toolchain:ctx.findlib_toolchain)
    in
    let open Action_builder.O in
    let entries =
      let+ () = install_file_deps
      and+ () =
        if strict_package_deps then
          Action_builder.map packages ~f:(fun (_ : Package.Id.Set.t) -> ())
        else Action_builder.return ()
      and+ entries = entries in
      let entries =
        match ctx.findlib_toolchain with
        | None -> entries
        | Some toolchain ->
          let toolchain = Context_name.to_string toolchain in
          let prefix = Path.of_string (toolchain ^ "-sysroot") in
          List.map entries ~f:(fun (e : Install.Entry.Sourced.t) ->
              { e with
                entry =
                  Install.Entry.add_install_prefix e.entry ~paths:install_paths
                    ~prefix
              })
      in
      (if not package.allow_empty then
       if
         List.for_all entries ~f:(fun (e : Install.Entry.Sourced.t) ->
             match e.source with
             | Dune -> true
             | User _ -> false)
       then
         let is_error = Dune_project.dune_version dune_project >= (3, 0) in
         User_warning.emit ~is_error
           [ Pp.textf
               "The package %s does not have any user defined stanzas attached \
                to it. If this is intentional, add (allow_empty) to the \
                package definition in the dune-project file"
               (Package.Name.to_string package_name)
           ]);
      List.map entries ~f:(fun (e : Install.Entry.Sourced.t) ->
          Install.Entry.set_src e.entry (Path.build e.entry.src))
    in
    Action_builder.with_file_targets ~file_targets:[ install_file ]
      (let+ entries = entries in
       let action = gen_install_file entries ~dst:install_file in
       Action.Full.make action)
  in

  Super_context.add_rule sctx ~dir:pkg_build_dir
    ~mode:
      (if promote_install_file ctx then
       Promote { lifetime = Until_clean; into = None; only = None }
      else
        (* We must ignore the source file since it might be copied to the source
           tree by another context. *)
        Ignore_source_files)
    action

let memo =
  Memo.create
    ~input:(module Super_context.As_memo_key.And_package)
    ~human_readable_description:(fun (_, pkg) ->
      Pp.textf "Computing installable artifacts for package %s"
        (Package.Name.to_string (Package.name pkg)))
    "install-rules-and-pkg-entries"
    (fun (sctx, pkg) ->
      Memo.return
        (let ctx = Super_context.context sctx in
         let context_name = ctx.name in
         Scheme.Approximation
           ( Dir_set.subtree (Local_install_path.dir ~context:context_name)
           , Thunk
               (fun () ->
                 let+ rules = symlinked_entries sctx pkg >>| snd in
                 let rules = Rules.of_rules rules in
                 Scheme.Finite (Rules.to_map rules)) )))

let scheme sctx pkg = Memo.exec memo (sctx, pkg)

let scheme_per_ctx_memo =
  Memo.create
    ~input:(module Super_context.As_memo_key)
    "install-rule-scheme"
    (fun sctx ->
      let* packages = Only_packages.get () in
      let packages = Package.Name.Map.values packages in
      let* schemes = Memo.sequential_map packages ~f:(scheme sctx) in
      Scheme.evaluate ~union:Rules.Dir_rules.union (Scheme.all schemes))

let symlink_rules sctx ~dir =
  let+ rules, subdirs =
    let* scheme = Memo.exec scheme_per_ctx_memo sctx in
    Scheme.Evaluated.get_rules scheme ~dir
  in
  ( Subdir_set.These subdirs
  , match rules with
    | None -> Rules.empty
    | Some rules -> Rules.of_dir_rules ~dir rules )

let gen_install_alias sctx (package : Package.t) =
  let ctx = Super_context.context sctx in
  let name = Package.name package in
  if ctx.implicit then Memo.return ()
  else
    let install_fn =
      install_file ~package:name ~findlib_toolchain:ctx.findlib_toolchain
    in
    let path = Package_paths.build_dir ctx package in
    let install_alias = Alias.install ~dir:path in
    let install_file = Path.relative (Path.build path) install_fn in
    Rules.Produce.Alias.add_deps install_alias
      (Action_builder.path install_file)

let gen_project_rules sctx project =
  let* () = meta_and_dune_package_rules sctx project in
  let* packages = Only_packages.packages_of_project project in
  Package.Name.Map_traversals.parallel_iter packages ~f:(fun _name package ->
      let* () = gen_package_install_file_rules sctx package in
      gen_install_alias sctx package)
