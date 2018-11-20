open! Stdune
open Import
open Dune_file
open Build.O
open! No_io

module type Params = sig
  val sctx : Super_context.t
end

module Gen(P : Params) = struct
  module Alias = Build_system.Alias
  module SC = Super_context
  open P

  let ctx = Super_context.context sctx

  let lib_dune_file ~dir ~name =
    Path.relative dir ((Lib_name.to_string name) ^ ".dune")

  let gen_lib_dune_file lib =
    SC.add_rule sctx
      (Build.arr (fun () ->
         let dune_version = Option.value_exn (Lib.dune_version lib) in
         Format.asprintf "%a@."
           (Dune_lang.pp (Stanza.File_kind.of_syntax dune_version))
           (Lib.Sub_system.dump_config lib
            |> Installed_dune_file.gen ~dune_version))
       >>> Build.write_file_dyn
             (lib_dune_file ~dir:(Lib.src_dir lib) ~name:(Lib.name lib)))

  let version_from_dune_project (pkg : Package.t) =
    let dir = Path.append (SC.build_dir sctx) pkg.path in
    let project = Scope.project (SC.find_scope_by_dir sctx dir) in
    Dune_project.version project

  type version_method =
    | File of string
    | From_dune_project

  let pkg_version path ~(pkg : Package.t) =
    match pkg.version_from_opam_file with
    | Some s -> Build.return (Some s)
    | None ->
      let rec loop = function
        | [] -> Build.return None
        | candidate :: rest ->
          match candidate with
          | File fn ->
            let p = Path.relative path fn in
            Build.if_file_exists p
              ~then_:(Build.lines_of p
                      >>^ function
                      | ver :: _ -> Some ver
                      | _ -> Some "")
              ~else_:(loop rest)
          | From_dune_project ->
            match version_from_dune_project pkg with
            | None -> loop rest
            | Some _ as x -> Build.return x
      in
      loop
        [ File (Package.Name.version_fn pkg.name)
        ; From_dune_project
        ; File "version"
        ; File "VERSION"
        ]

  let init_meta (pkg : Local_package.t) =
    let libs = Local_package.libs pkg in
    Lib.Set.iter libs ~f:(gen_lib_dune_file ~dir:ctx.build_dir);
    let path = Local_package.build_dir pkg in
    let pkg_name = Local_package.name pkg in
    let meta = Local_package.meta_file pkg in
    let pkg = Local_package.package pkg in
    SC.on_load_dir sctx ~dir:path ~f:(fun () ->
      let meta_template = Path.extend_basename meta ~suffix:".template" in

      let version =
        let get = pkg_version ~pkg path in
        Super_context.Pkg_version.set sctx pkg get
      in

      let template =
        Build.if_file_exists meta_template
          ~then_:(Build.lines_of meta_template)
          ~else_:(Build.return ["# DUNE_GEN"])
      in
      let meta_contents =
        version >>^ fun version ->
        Gen_meta.gen
          ~package:(Package.Name.to_string pkg_name)
          ~version
          (Lib.Set.to_list libs)
      in
      SC.add_rule sctx ~dir:ctx.build_dir
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

  let lib_ppxs ~(lib : Dune_file.Library.t) ~scope ~dir_kind =
    match lib.kind with
    | Normal | Ppx_deriver -> []
    | Ppx_rewriter ->
      let name = Dune_file.Library.best_name lib in
      match (dir_kind : Dune_lang.Syntax.t) with
      | Dune ->
        [Preprocessing.get_compat_ppx_exe sctx ~name ~kind:Dune]
      | Jbuild ->
        let driver =
          let deps =
            List.concat_map lib.buildable.libraries ~f:Lib_dep.to_lib_names
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

  let lib_install_files ~dir_contents ~dir ~sub_dir ~scope ~dir_kind
        (lib : Library.t) =
    let make_entry section ?dst fn =
      Install.Entry.make section fn
        ~dst:(
          let dst =
            match dst with
            | Some s -> s
            | None   -> Path.basename fn
          in
          match sub_dir with
          | None -> dst
          | Some dir -> sprintf "%s/%s" dir dst)
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
          let dst = Path.basename source |> String.drop_suffix ~suffix:"-gen" in
          make_entry Lib source ?dst))
    in
    let module_files =
      let if_ cond l = if cond then l else [] in
      let (_loc, lib_name_local) = lib.name in
      let obj_dir = Utils.library_object_directory ~dir lib_name_local in
      let { Mode.Dict.byte = _; native } =
        Dune_file.Mode_conf.Set.eval lib.modes
          ~has_native:(Option.is_some ctx.ocamlopt)
      in
      let virtual_library = Library.is_virtual lib in
      List.concat_map installable_modules ~f:(fun m ->
        List.concat
          [ if_ (Module.is_public m)
              [ Module.cm_file_unsafe m ~obj_dir Cmi ]
          ; if_ (native && Module.has_impl m)
              [ Module.cm_file_unsafe m ~obj_dir Cmx ]
          ; if_ (native && Module.has_impl m && virtual_library)
              [ Module.obj_file m ~obj_dir ~ext:ctx.ext_obj ]
          ; List.filter_map Ml_kind.all ~f:(Module.cmt_file m ~obj_dir)
          ])
    in
    let archives = Lib_archives.make ~ctx ~dir lib in
    let execs = lib_ppxs ~lib ~scope ~dir_kind in
    List.concat
      [ sources
      ; List.map module_files ~f:(make_entry Lib)
      ; List.map (Lib_archives.files archives) ~f:(make_entry Lib)
      ; List.map execs ~f:(make_entry Libexec)
      ; List.map (Lib_archives.dlls archives) ~f:(Install.Entry.make Stublibs)
      ; [make_entry Lib (lib_dune_file ~dir
                           ~name:(Dune_file.Library.best_name lib))]
      ]

  let local_install_rules (entries : Install.Entry.t list)
        ~install_paths ~package =
    let install_dir = Config.local_install_dir ~context:ctx.name in
    List.map entries ~f:(fun entry ->
      let dst =
        Path.append install_dir
          (Install.Entry.relative_installed_path entry ~paths:install_paths)
      in
      Build_system.set_package (SC.build_system sctx) entry.src package;
      SC.add_rule sctx ~dir:ctx.build_dir (Build.symlink ~src:entry.src ~dst);
      Install.Entry.set_src entry dst)

  let promote_install_file =
    not ctx.implicit &&
    match ctx.kind with
    | Default -> true
    | Opam _  -> false

  let install_file (package : Local_package.t) entries =
    let opam = Local_package.opam_file package in
    let meta = Local_package.meta_file package in
    let package_name = Local_package.name package in
    let pkg_build_dir = Local_package.build_dir package in
    let install_paths = Local_package.install_paths package in
    let entries =
      let docs =
        Local_package.odig_files package
        |> List.map ~f:(fun doc -> Install.Entry.make Doc doc)
      in
      local_install_rules ~package:package_name ~install_paths (
        Install.Entry.make Lib opam ~dst:"opam"
        :: Install.Entry.make Lib meta ~dst:"META"
        :: docs)
      |> List.rev_append entries
    in
    let fn =
      Path.relative pkg_build_dir
        (Utils.install_file ~package:package_name
           ~findlib_toolchain:ctx.findlib_toolchain)
    in
    let files = Install.files entries in
    SC.add_alias_deps sctx
      (Alias.package_install ~context:ctx ~pkg:package_name)
      files
      ~dyn_deps:
        (Build_system.package_deps (SC.build_system sctx) package_name files
         >>^ fun packages ->
         Package.Name.Set.to_list packages
         |> List.map ~f:(fun pkg ->
           Build_system.Alias.package_install ~context:ctx ~pkg
           |> Build_system.Alias.stamp_file)
         |> Path.Set.of_list);
    SC.add_rule sctx ~dir:pkg_build_dir
      ~mode:(if promote_install_file then
               Promote_but_delete_on_clean
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
       Build.write_file_dyn fn)

  let init_install (package : Local_package.t) =
    let installs =
      Local_package.installs package
      |> List.concat_map
           ~f:(fun ({ Installable.
                      data = { Install_conf. section; files; package = _ }
                    ; dir
                    ; scope = _
                    ; kind = _ }) ->
                List.map files ~f:(fun {File_bindings. src; dst } ->
                  Install.Entry.make section (Path.relative dir src) ?dst))
    in
    let docs =
      Local_package.mlds package
      |> List.map ~f:(fun mld ->
        (Install.Entry.make
           ~dst:(sprintf "odoc-pages/%s" (Path.basename mld))
           Install.Section.Doc mld))
    in
    let lib_install_files =
      Local_package.lib_stanzas package
      |> List.concat_map
           ~f:(fun { Installable.
                     data = (lib : Dune_file.Library.t)
                   ; scope
                   ; dir
                   ; kind = dir_kind
                   } ->
                let sub_dir =
                  (Option.value_exn lib.public).sub_dir in
                let dir_contents = Dir_contents.get sctx ~dir in
                lib_install_files ~dir ~sub_dir lib ~scope
                  ~dir_kind ~dir_contents)
    in
    let package_name = Local_package.name package in
    let install_paths = Local_package.install_paths package in
    let entries =
      local_install_rules ~package:package_name ~install_paths
        (installs @ docs @ lib_install_files)
    in
    install_file package entries

  let init_install_files (package : Local_package.t) =
    if not ctx.implicit then
          let install_fn =
            Utils.install_file ~package:(Local_package.name package)
              ~findlib_toolchain:ctx.findlib_toolchain
          in

          let path = Local_package.build_dir package in
          let install_alias = Alias.install ~dir:path in
          let install_file = Path.relative path install_fn in
          SC.add_alias_deps sctx install_alias (Path.Set.singleton install_file)

  let init () =
    Local_package.of_sctx sctx
    |> Package.Name.Map.iter ~f:(fun pkg ->
      init_meta pkg;
      init_install pkg;
      init_install_files pkg)
end
