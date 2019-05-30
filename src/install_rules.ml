open! Stdune
open Import
open Build.O
open! No_io

module Library = Dune_file.Library

let gen_dune_package sctx ~version ~(pkg : Local_package.t) =
  let ctx = Super_context.context sctx in
  let dune_package_file = Local_package.dune_package_file pkg in
  let meta_template = Local_package.meta_template pkg in
  let name = Local_package.name pkg in
  let dune_version = Syntax.greatest_supported_version Stanza.syntax in
  Build.if_file_exists (Path.build meta_template)
    ~then_:(Build.return Dune_package.Or_meta.Use_meta)
    ~else_:(
      version >>^ (fun version ->
        let dune_package =
          let pkg_root =
            Config.local_install_lib_dir ~context:ctx.name ~package:name
          in
          let lib_root lib =
            let (_, subdir) = Lib_name.split (Lib.name lib) in
            Path.Build.L.relative pkg_root subdir
          in
          let libs =
            Local_package.libs pkg
            |> Lib.Set.to_list
            |> List.map ~f:(fun lib ->
              let name = Lib.name lib in
              let dir_contents =
                Dir_contents.get_without_rules sctx ~dir:(
                  Path.as_in_build_dir_exn (Lib.src_dir lib)) in
              let lib_modules =
                Dir_contents.modules_of_library dir_contents ~name in
              let foreign_objects =
                let dir = Obj_dir.obj_dir (Lib.obj_dir lib) in
                Dir_contents.c_sources_of_library dir_contents ~name
                |> C.Sources.objects ~dir:(Path.as_in_build_dir_exn dir)
                     ~ext_obj:ctx.ext_obj
                |> List.map ~f:Path.build
              in
              Lib.to_dune_lib lib ~dir:(Path.build (lib_root lib)) ~lib_modules
                ~foreign_objects)
          in
          Dune_package.Or_meta.Dune_package
            { Dune_package.
              version
            ; name
            ; libs
            ; dir = Path.build pkg_root
            }
        in
        dune_package))
  >>^ (fun pkg ->
    Dune_package.Or_meta.encode ~dune_version pkg
    |> Format.asprintf "%a@."
         (Fmt.list ~pp_sep:Fmt.nl
            (Dune_lang.pp (Stanza.File_kind.of_syntax dune_version))))
  >>>
  Build.write_file_dyn  dune_package_file
  |> Super_context.add_rule sctx ~dir:ctx.build_dir

type version_method =
  | File of string
  | From_metadata of Package.Version_source.t

let pkg_version ~path ~(pkg : Package.t) =
  let rec loop = function
    | [] -> Build.return None
    | candidate :: rest ->
      match candidate with
      | File fn ->
        let p = Path.Build.relative path fn |> Path.build in
        Build.if_file_exists p
          ~then_:(Build.lines_of p
                  >>^ function
                  | ver :: _ -> Some ver
                  | _ -> Some "")
          ~else_:(loop rest)
      | From_metadata source ->
        match pkg.version with
        | Some (v, source') when source = source' ->
          Build.return (Some v)
        | _ -> loop rest
  in
  loop
    [ From_metadata Package
    ; File (Package.Name.version_fn pkg.name)
    ; From_metadata Project
    ; File "version"
    ; File "VERSION"
    ]

let init_meta sctx ~dir =
  Local_package.defined_in sctx ~dir
  |> List.iter ~f:(fun pkg ->
    let libs = Local_package.libs pkg in
    let path = Local_package.build_dir pkg in
    let pkg_name = Local_package.name pkg in
    let meta = Local_package.meta_file pkg in
    let meta_template = Path.build (Local_package.meta_template pkg) in
    let version =
      let pkg = Local_package.package pkg in
      let get = pkg_version ~pkg ~path in
      Super_context.Pkg_version.set sctx pkg get
    in

    gen_dune_package sctx ~version ~pkg;

    let template =
      Build.if_file_exists meta_template
        ~then_:(
          match Local_package.virtual_lib pkg with
          | Some lib ->
            Build.fail { fail = fun () ->
              Errors.fail (Loc.in_file meta_template)
                "Package %a defines virtual library %a and has a META \
                 template. This is not allowed."
                Package.Name.pp (Local_package.name pkg)
                Lib_name.pp (Lib.name lib)
            }
          | None ->
            Build.lines_of meta_template)
        ~else_:(Build.return ["# DUNE_GEN"])
    in
    let meta_contents =
      version >>^ fun version ->
      Gen_meta.gen
        ~package:(Package.Name.to_string pkg_name)
        ~version
        (Lib.Set.to_list libs)
    in
    let ctx = Super_context.context sctx in
    Super_context.add_rule sctx ~dir:ctx.build_dir
      (Build.fanout meta_contents template
       >>^ (fun ((meta : Meta.t), template) ->
         let buf = Buffer.create 1024 in
         let ppf = Format.formatter_of_buffer buf in
         Format.pp_open_vbox ppf 0;
         List.iter template ~f:(fun s ->
           if String.is_prefix s ~prefix:"#" then
             match
               String.extract_blank_separated_words (String.drop s 1)
             with
             | ["JBUILDER_GEN" | "DUNE_GEN"] ->
               Format.fprintf ppf "%a@," Meta.pp meta.entries
             | _ -> Format.fprintf ppf "%s@," s
           else
             Format.fprintf ppf "%s@," s);
         Format.pp_close_box ppf ();
         Format.pp_print_flush ppf ();
         Buffer.contents buf)
       >>>
       Build.write_file_dyn meta))

let lib_ppxs sctx ~(lib : Dune_file.Library.t) ~scope ~dir_kind =
  match lib.kind with
  | Normal | Ppx_deriver _ -> []
  | Ppx_rewriter _ ->
    let name = Dune_file.Library.best_name lib in
    match (dir_kind : Dune_lang.File_syntax.t) with
    | Dune ->
      [Preprocessing.get_compat_ppx_exe sctx ~name ~kind:Dune]
    | Jbuild ->
      let driver =
        let deps =
          List.concat_map lib.buildable.libraries
            ~f:Dune_file.Lib_dep.to_lib_names
        in
        match
          List.filter deps ~f:(fun lib_name ->
            match Lib_name.to_string lib_name with
            | "ppx_driver" | "ppxlib" | "ppx_type_conv" -> true
            | _ -> false)
        with
        | [] -> None
        | l ->
          match Scope.name scope
              , List.mem ~set:l (Lib_name.of_string_exn ~loc:None "ppxlib")
          with
          | Named "ppxlib", _ | _, true ->
            Some "ppxlib.runner"
          | _ ->
            Some "ppx_driver.runner"
      in
      [Preprocessing.get_compat_ppx_exe sctx ~name ~kind:(Jbuild driver)]

let lib_install_files sctx ~dir_contents ~dir ~sub_dir:lib_subdir
      ~scope ~dir_kind (lib : Library.t) =
  let loc = lib.buildable.loc in
  let make_entry section ?sub_dir ?dst fn =
    ( Some loc
    , Install.Entry.make section fn
        ~dst:(
          let dst =
            match dst with
            | Some s -> s
            | None   -> Path.Build.basename fn
          in
          let sub_dir =
            match sub_dir with
            | Some _ -> sub_dir
            | None -> lib_subdir
          in
          match sub_dir with
          | None -> dst
          | Some dir -> sprintf "%s/%s" dir dst))
  in
  let installable_modules =
    Dir_contents.modules_of_library dir_contents
      ~name:(Library.best_name lib)
    |> Lib_modules.installable_modules
  in
  let sources =
    List.concat_map installable_modules ~f:(fun m ->
      List.map (Module.sources m) ~f:(fun source ->
        (* We add the -gen suffix to a few files generated by dune,
           such as the alias module. *)
        let source = Path.as_in_build_dir_exn source in
        let dst =
          Path.Build.basename source
          |> String.drop_suffix ~suffix:"-gen" in
        make_entry Lib source ?dst))
  in
  let ctx = Super_context.context sctx in
  let module_files =
    let if_ cond l = if cond then l else [] in
    let { Mode.Dict.byte ; native } =
      Dune_file.Mode_conf.Set.eval lib.modes
        ~has_native:(Option.is_some ctx.ocamlopt)
    in
    let virtual_library = Library.is_virtual lib in
    List.concat_map installable_modules ~f:(fun m ->
      let cmi_file =
        ( Module.visibility m
        , Path.as_in_build_dir_exn (Module.cm_file_unsafe m Cmi)
        )
      in
      let other_cm_files =
        [ if_ (native && Module.has_impl m)
            [ Module.cm_file_unsafe m Cmx ]
        ; if_ (byte && Module.has_impl m && virtual_library)
            [ Module.cm_file_unsafe m Cmo ]
        ; if_ (native && Module.has_impl m && virtual_library)
            [ Module.obj_file m ~kind:Cmx ~ext:ctx.ext_obj ]
        ; List.filter_map Ml_kind.all ~f:(Module.cmt_file m)
        ]
        |> List.concat
        |> List.map ~f:(fun f ->
          (Visibility.Public, Path.as_in_build_dir_exn f))
      in
      cmi_file :: other_cm_files
    )
  in
  let archives = Lib_archives.make ~ctx ~dir_contents ~dir lib in
  let execs = lib_ppxs sctx ~lib ~scope ~dir_kind in
  List.concat
    [ sources
    ; List.map module_files ~f:(fun (visibility, file) ->
        let sub_dir =
          match (visibility : Visibility.t), lib_subdir with
          | Public, _ -> lib_subdir
          | Private, None -> Some ".private"
          | Private, Some dir -> Some (Filename.concat dir ".private")
        in
        make_entry ?sub_dir Lib file)
    ; List.map (Lib_archives.files archives) ~f:(make_entry Lib)
    ; List.map execs ~f:(make_entry Libexec)
    ; List.map (Lib_archives.dlls archives) ~f:(fun a ->
        (Some loc, Install.Entry.make Stublibs a))
    ]

let symlink_installed_artifacts_to_build_install
      sctx (entries : (Loc.t option * Install.Entry.t) list)
      ~install_paths =
  let ctx = Super_context.context sctx in
  let install_dir = Config.local_install_dir ~context:ctx.name in
  List.map entries ~f:(fun (loc, entry) ->
    let dst =
      Path.append (Path.build install_dir)
        (Install.Entry.relative_installed_path entry ~paths:install_paths)
      |> Path.as_in_build_dir_exn
    in
    let loc =
      match loc with
      | Some l -> l
      | None -> Loc.in_file (Path.build entry.src)
    in
    Super_context.add_rule sctx ~loc ~dir:ctx.build_dir
      (Build.symlink ~src:(Path.build entry.src) ~dst);
    Install.Entry.set_src entry dst)

let promote_install_file (ctx : Context.t) =
  not ctx.implicit &&
  match ctx.kind with
  | Default -> true
  | Opam _  -> false

module Sctx_and_package = struct
  type t = Super_context.t * Local_package.t

  let hash (x, y) = Hashtbl.hash (Super_context.hash x, Local_package.hash y)
  let equal (x1, y1) (x2, y2) = (x1 == x2 && y1 == y2)
  let to_sexp _ = Sexp.Atom "<opaque>"
end

let install_entries sctx package =
  let installs =
    Local_package.installs package
    |> List.concat_map ~f:(fun (d : _ Dir_with_dune.t) ->
      let { Dune_file.Install_conf. section; files; package = _ } =
        d.data
      in
      List.map files ~f:(fun fb ->
        let loc = File_binding.Expanded.src_loc fb in
        let src = Path.as_in_build_dir_exn (File_binding.Expanded.src fb) in
        let dst = File_binding.Expanded.dst fb in
        ( Some loc
        , Install.Entry.make section src ?dst
        )))
  in
  let docs =
    (Local_package.mlds package
     |> List.map ~f:(fun mld ->
       (None,
        Install.Entry.make
          ~dst:(sprintf "odoc-pages/%s" (Path.Build.basename mld))
          Install.Section.Doc mld)))
    @
    (Local_package.odig_files package
     |> List.map ~f:(fun doc -> (None, Install.Entry.make Doc doc)))
  in
  let lib_install_files =
    Local_package.lib_stanzas package
    |> List.concat_map
         ~f:(fun { Dir_with_dune.
                   data = (lib : Dune_file.Library.t)
                 ; scope
                 ; ctx_dir = dir
                 ; src_dir = _
                 ; kind = dir_kind
                 ; dune_version = _
                 } ->
              let sub_dir = (Option.value_exn lib.public).sub_dir in
              let dir_contents = Dir_contents.get_without_rules sctx ~dir in
              lib_install_files sctx ~dir ~sub_dir lib ~scope
                ~dir_kind ~dir_contents)
  in
  let coqlib_install_files =
    Local_package.coqlibs package
    |> List.concat_map
         ~f:(fun { Dir_with_dune.
                   data = coqlib
                 ; ctx_dir = dir
                 ; _
                 } ->
              Coq_rules.install_rules ~sctx ~dir coqlib)
  in
  let metadata =
    let meta = Local_package.meta_file package in
    let dune_package = Local_package.dune_package_file package in
    let opam_file = Local_package.opam_file package in
    (None, Install.Entry.make Lib meta ~dst:"META")
    :: (None, Install.Entry.make Lib dune_package
                ~dst:"dune-package")
    :: (let package = Local_package.package package in
        match package.kind with
        | Dune false -> []
        | Dune true
        | Opam ->
          [(None, Install.Entry.make Lib opam_file ~dst:"opam")])
  in
  coqlib_install_files
  |> List.rev_append lib_install_files
  |> List.rev_append installs
  |> List.rev_append docs
  |> List.rev_append metadata

let install_entries =
  let memo =
    Memo.create
      ~input:(module Sctx_and_package)
      ~output:(
        Simple (module struct
          type t = (Loc.t option * Install.Entry.t) list
          let to_sexp _ = Sexp.Atom "<opaque>"
        end))
      "install-entries"
      ~doc:"install entries"
      ~visibility:Hidden
      Sync
      (Some (fun (sctx, package) -> install_entries sctx package))
  in
  fun sctx package -> Memo.exec memo (sctx, package)

let package_source_files sctx package =
  List.map
    ~f:(fun (_loc, entry) -> entry.Install.Entry.src)
    (install_entries sctx package)

let install_rules sctx package =
  let install_paths = Local_package.install_paths package in
  let entries =
    install_entries sctx package
    |> symlink_installed_artifacts_to_build_install sctx ~install_paths
  in
  let ctx = Super_context.context sctx in
  let package_name = Local_package.name package in
  let pkg_build_dir = Local_package.build_dir package in
  let install_file =
    Path.Build.relative
      pkg_build_dir
      (Utils.install_file ~package:package_name
         ~findlib_toolchain:ctx.findlib_toolchain)
  in
  let files = Install.files entries in
  let target_alias =
    Build_system.Alias.package_install ~context:ctx ~pkg:package_name
  in
  let () =
    Rules.Produce.Alias.add_deps
      target_alias
      files
      ~dyn_deps:
        (Build_system.package_deps package_name files
         >>^ fun packages ->
         Package.Name.Set.to_list packages
         |> List.map ~f:(fun pkg ->
           Build_system.Alias.package_install ~context:ctx ~pkg
           |> Alias.stamp_file)
         |> Path.Set.of_list)
  in
  Super_context.add_rule sctx ~dir:pkg_build_dir
    ~mode:(if promote_install_file ctx then
             Promote { lifetime = Until_clean ; into = None; only = None }
           else
             (* We must ignore the source file since it might be
                copied to the source tree by another context. *)
             Ignore_source_files)
    (Build.path_set files
     >>^ (fun () ->
       let entries =
         match ctx.findlib_toolchain with
         | None -> entries
         | Some toolchain ->
           let prefix = Path.of_string (toolchain ^ "-sysroot") in
           List.map entries
             ~f:(Install.Entry.add_install_prefix
                   ~paths:install_paths ~prefix)
       in
       Install.gen_install_file entries)
     >>>
     Build.write_file_dyn install_file)

let install_alias (ctx : Context.t) (package : Local_package.t) =
  if not ctx.implicit then
    let install_fn =
      Utils.install_file ~package:(Local_package.name package)
        ~findlib_toolchain:ctx.findlib_toolchain
    in
    let path = Path.build (Local_package.build_dir package) in
    let install_alias = Alias.install ~dir:path in
    let install_file = Path.relative path install_fn in
    Rules.Produce.Alias.add_deps install_alias (Path.Set.singleton install_file)

module Scheme' =struct

  type t = Rules.Dir_rules.t Scheme.t

  let to_sexp _ = Sexp.Atom "<opaque>"
end

let memo =
  Memo.create
    ~input:(module Sctx_and_package)
    ~output:(Simple (module Scheme'))
    "install-rules-and-pkg-entries"
    ~doc:"install rules and package entries"
    ~visibility:Hidden
    Sync
    (Some (fun (sctx, pkg) ->
       let ctx = Super_context.context sctx in
       let context_name = ctx.name in
       let rules = Memo.lazy_ (fun () ->
         Rules.collect_unit (fun () ->
           install_rules sctx pkg;
           install_alias ctx pkg
         ))
       in
       (
         Approximation (
           (Dir_set.union_all
              [
                Dir_set.subtree
                  (Config.local_install_dir ~context:context_name);
                Dir_set.singleton (Local_package.build_dir pkg);
                Dir_set.singleton ctx.build_dir
              ])
           ,
           Thunk (fun () -> Finite (
             Rules.to_map (Memo.Lazy.force rules)))
         )
       )))

let scheme sctx pkg = Memo.exec memo (sctx, pkg)

let scheme_per_ctx_memo =
  Memo.create
    ~input:(module Super_context)
    ~output:
      (Simple (module struct
         type t = Rules.Dir_rules.t Scheme.Evaluated.t
         let to_sexp _ = Sexp.Atom "<opaque>"
       end ))
    "install-rule-scheme"
    ~doc:"install rules scheme"
    ~visibility:Hidden
    Sync
    (Some (fun sctx ->
       let packages = Local_package.of_sctx sctx in
       let scheme =
         Scheme.all (
           List.map (Package.Name.Map.to_list packages)
             ~f:(fun (_, pkg) -> (scheme sctx pkg)))
       in
       Scheme.evaluate ~union:Rules.Dir_rules.union scheme))

let gen_rules sctx ~dir =
  let rules =
    Scheme.Evaluated.get_rules (Memo.exec scheme_per_ctx_memo sctx) ~dir
    |> Option.value ~default:Rules.Dir_rules.empty
  in
  Rules.produce_dir ~dir rules

let packages =
  let f sctx =
    Package.Name.Map.foldi (Local_package.of_sctx sctx)
      ~init:[]
      ~f:(fun name pkg acc ->
        List.fold_left (package_source_files sctx pkg)
          ~init:acc ~f:(fun acc path -> (path, name) :: acc))
    |> Path.Build.Map.of_list_fold
         ~init:Package.Name.Set.empty
         ~f:Package.Name.Set.add
  in
  let memo =
    Memo.create "package-map"
      ~doc:"Return a map assining package to files"
      ~input:(module Super_context)
      ~visibility:Hidden
      ~output:(Allow_cutoff (module struct
                 type t = Package.Name.Set.t Path.Build.Map.t
                 let to_sexp =
                   Map.to_sexp
                     Path.Build.Map.to_list
                     Path.Build.to_sexp
                     (Set.to_sexp Package.Name.Set.to_list Package.Name.to_sexp)
                 let equal =
                   Path.Build.Map.equal
                     ~equal:Package.Name.Set.equal
               end))
      Sync
      (Some f)
  in
  fun sctx -> Memo.exec memo sctx
