open Import
open Jbuild
open Build.O
open! No_io

module type Params = sig
  val sctx : Super_context.t
end

module type Install_params = sig
  include Params
  val module_names_of_lib : Library.t -> dir:Path.t -> Module.t list
  val mlds_of_dir : Documentation.t -> dir:Path.t -> Path.t list
end

module Archives(P : Params) = struct
  let ctx = Super_context.context P.sctx

  let lib_archive (lib : Library.t) ~dir ~ext =
    Path.relative dir (lib.name ^ ext)

  let stubs_archive lib ~dir =
    Library.stubs_archive lib ~dir ~ext_lib:ctx.ext_lib

  let dll (lib : Library.t) ~dir =
    Path.relative dir (sprintf "dll%s_stubs%s" lib.name ctx.ext_dll)
end

module Gen(P : Install_params) = struct
  module Alias = Build_system.Alias
  module SC = Super_context
  open P

  include (Archives(P))

  let lib_dune_file ~dir ~name =
    Path.relative dir (name ^ ".dune")

  let gen_lib_dune_file lib =
    let config = Lib.Sub_system.dump_config lib in
    if not (Sub_system_name.Map.is_empty config) then
      SC.add_rule sctx
        (Build.arr (fun () ->
           Format.asprintf "%a@." Sexp.pp
             (Lib.Sub_system.dump_config lib |> Installed_dune_file.gen))
         >>> Build.write_file_dyn
               (lib_dune_file ~dir:(Lib.src_dir lib) ~name:(Lib.name lib)))

  let init_meta () =
    SC.libs_by_package sctx
    |> Package.Name.Map.iter ~f:(fun ((pkg : Package.t), libs) ->
        Lib.Set.iter libs ~f:gen_lib_dune_file;
      let path = Path.append ctx.build_dir pkg.path in
      SC.on_load_dir sctx ~dir:path ~f:(fun () ->
        let meta_fn = "META." ^ (Package.Name.to_string pkg.name) in

        let meta_template = Path.relative path (meta_fn ^ ".template"     ) in
        let meta          = Path.relative path  meta_fn                     in

        let version =
          let get =
            match pkg.version_from_opam_file with
            | Some s -> Build.return (Some s)
            | None ->
              let rec loop = function
                | [] -> Build.return None
                | candidate :: rest ->
                  let p = Path.relative path candidate in
                  Build.if_file_exists p
                    ~then_:(Build.lines_of p
                            >>^ function
                            | ver :: _ -> Some ver
                            | _ -> Some "")
                    ~else_:(loop rest)
              in
              loop
                [ (Package.Name.to_string pkg.name) ^ ".version"
                ; "version"
                ; "VERSION"
                ]
          in
          Super_context.Pkg_version.set sctx pkg get
        in

        let template =
          Build.if_file_exists meta_template
            ~then_:(Build.lines_of meta_template)
            ~else_:(Build.return ["# JBUILDER_GEN"])
        in
        let meta_contents =
          version >>^ fun version ->
          Gen_meta.gen
            ~package:(Package.Name.to_string pkg.name)
            ~version
            (Lib.Set.to_list libs)
        in
        SC.add_rule sctx
          (Build.fanout meta_contents template
           >>^ (fun ((meta : Meta.t), template) ->
             let buf = Buffer.create 1024 in
             let ppf = Format.formatter_of_buffer buf in
             Format.pp_open_vbox ppf 0;
             List.iter template ~f:(fun s ->
               if String.is_prefix s ~prefix:"#" then
                 match
                   String.extract_blank_separated_words
                     (String.sub s ~pos:1 ~len:(String.length s - 1))
                 with
                 | ["JBUILDER_GEN"] -> Format.fprintf ppf "%a@," Meta.pp meta.entries
                 | _ -> Format.fprintf ppf "%s@," s
               else
                 Format.fprintf ppf "%s@," s);
             Format.pp_close_box ppf ();
             Format.pp_print_flush ppf ();
             Buffer.contents buf)
           >>>
           Build.write_file_dyn meta)))

  let lib_install_files ~dir ~sub_dir ~scope ~name (lib : Library.t) =
    let obj_dir = Utils.library_object_directory ~dir lib.name in
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
    let { Mode.Dict.byte; native } =
      Mode_conf.Set.eval lib.modes
        ~has_native:(Option.is_some ctx.ocamlopt)
    in
    let if_ cond l = if cond then l else [] in
    let files =
      let modules = module_names_of_lib lib ~dir in
      List.concat
        [ List.concat_map modules ~f:(fun m ->
            List.concat
              [ [ Module.cm_file_unsafe m ~obj_dir Cmi ]
              ; if_ (native && Module.has_impl m)
                  [ Module.cm_file_unsafe m ~obj_dir Cmx ]
              ; List.filter_map Ml_kind.all ~f:(Module.cmt_file m ~obj_dir)
              ; List.filter_map [m.intf;m.impl] ~f:(function
                  | None -> None
                  | Some f -> Some (Path.relative dir f.name))
              ])
        ; if_ byte [ lib_archive ~dir lib ~ext:".cma" ]
        ; if_ (Library.has_stubs lib) [ stubs_archive ~dir lib ]
        ; if_ native
            (let files =
               [ lib_archive ~dir lib ~ext:".cmxa"
               ; lib_archive ~dir lib ~ext:ctx.ext_lib
               ]
             in
             if ctx.natdynlink_supported && lib.dynlink then
               files @ [ lib_archive ~dir lib ~ext:".cmxs" ]
             else
               files)
        ; List.map lib.buildable.js_of_ocaml.javascript_files ~f:(Path.relative dir)
        ; List.map lib.install_c_headers ~f:(fun fn ->
            Path.relative dir (fn ^ ".h"))
        ]
    in
    let dlls  = if_ (byte && Library.has_stubs lib && lib.dynlink) [dll ~dir lib] in
    let execs =
      match lib.kind with
      | Normal | Ppx_deriver -> []
      | Ppx_rewriter ->
        let pps = [(lib.buildable.loc, Pp.of_string lib.name)] in
        let pps =
          (* This is a temporary hack until we get a standard driver *)
          let deps =
            List.concat_map lib.buildable.libraries ~f:Lib_dep.to_lib_names
          in
          if List.exists deps ~f:(function
            | "ppx_driver" | "ppx_type_conv" -> true
            | _ -> false) then
            pps @ [match Scope.name scope with
              | Some "ppxlib" ->
                Loc.none, Pp.of_string "ppxlib.runner"
              | _ ->
                Loc.none, Pp.of_string "ppx_driver.runner"]
          else
            pps
        in
        let ppx_exe = Preprocessing.get_ppx_driver sctx ~scope pps in
        [ppx_exe]
    in
    let requires_installed_dune_file =
      Sub_system_name.Map.existsi lib.sub_systems ~f:(fun name _ ->
        Lib.Sub_system.requires_installed_dune_file name)
    in
    List.concat
      [ List.map files ~f:(make_entry Lib    )
      ; List.map execs ~f:(make_entry Libexec)
      ; List.map dlls  ~f:(Install.Entry.make Stublibs)
      ; (if requires_installed_dune_file then
           [make_entry Lib (lib_dune_file ~dir ~name)]
         else
           [])
      ]

  let is_odig_doc_file fn =
    List.exists [ "README"; "LICENSE"; "CHANGE"; "HISTORY"]
      ~f:(fun prefix -> String.is_prefix fn ~prefix)

  let local_install_rules (entries : Install.Entry.t list) ~package =
    let install_dir = Config.local_install_dir ~context:ctx.name in
    List.map entries ~f:(fun entry ->
      let dst =
        Path.append install_dir
          (Install.Entry.relative_installed_path entry ~package)
      in
      Build_system.set_package (SC.build_system sctx) entry.src package;
      SC.add_rule sctx (Build.symlink ~src:entry.src ~dst);
      Install.Entry.set_src entry dst)

  let promote_install_file =
    not ctx.implicit &&
    match ctx.kind with
    | Default -> true
    | Opam _  -> false

  let install_file package_path package entries =
    let entries =
      let files = SC.source_files sctx ~src_path:Path.root in
      String_set.fold files ~init:entries ~f:(fun fn acc ->
        if is_odig_doc_file fn then
          Install.Entry.make Doc (Path.relative ctx.build_dir fn) :: acc
        else
          acc)
    in
    let entries =
      let opam = Path.relative package_path (Package.Name.opam_fn package) in
      Install.Entry.make Lib opam ~dst:"opam" :: entries
    in
    let entries =
      let meta_fn = "META." ^ (Package.Name.to_string package) in
      let meta = Path.append ctx.build_dir (Path.relative package_path meta_fn) in
      Install.Entry.make Lib meta ~dst:"META" :: entries
    in
    let fn =
      Path.relative (Path.append ctx.build_dir package_path)
        (Utils.install_file ~package ~findlib_toolchain:ctx.findlib_toolchain)
    in
    let entries = local_install_rules entries ~package in
    let files = Install.files entries in
    SC.add_alias_deps sctx
      (Alias.package_install ~context:ctx ~pkg:package)
      files
      ~dyn_deps:
        (Build_system.package_deps (SC.build_system sctx) package files
         >>^ fun packages ->
         Package.Name.Set.to_list packages
         |> List.map ~f:(fun pkg ->
           Build_system.Alias.package_install
             ~context:(SC.context sctx) ~pkg
           |> Build_system.Alias.stamp_file)
         |> Path.Set.of_list);
    SC.add_rule sctx
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
               ~f:(Install.Entry.add_install_prefix ~prefix ~package)
         in
         Install.gen_install_file entries)
       >>>
       Build.write_file_dyn fn)

  let init_install () =
    let entries_per_package =
      List.concat_map (SC.stanzas_to_consider_for_install sctx)
        ~f:(fun (dir, scope, stanza) ->
          match stanza with
          | Library ({ public = Some { package; sub_dir; name; _ }; _ } as lib) ->
            List.map (lib_install_files ~dir ~sub_dir ~scope ~name lib)
              ~f:(fun x -> package.name, x)
          | Install { section; files; package}->
            List.map files ~f:(fun { Install_conf. src; dst } ->
              (package.name,
               Install.Entry.make section (Path.relative dir src) ?dst))
          | Documentation ({ package; _ } as d) ->
            List.map ~f:(fun mld ->
              (package.name,
               (Install.Entry.make
                  ~dst:(sprintf "odoc-pages/%s" (Path.basename mld))
                  Install.Section.Doc mld))
            ) (mlds_of_dir d ~dir)
          | _ -> [])
      |> Package.Name.Map.of_list_multi
    in
    Package.Name.Map.iter (SC.packages sctx) ~f:(fun (pkg : Package.t) ->
      let stanzas =
        Option.value (Package.Name.Map.find entries_per_package pkg.name)
          ~default:[]
      in
      install_file pkg.path pkg.name stanzas)

  let init_install_files () =
    if not ctx.implicit then
      Package.Name.Map.iteri (SC.packages sctx)
        ~f:(fun pkg { Package.path = src_path; _ } ->
          let install_fn =
            Utils.install_file ~package:pkg
              ~findlib_toolchain:ctx.findlib_toolchain
          in

          let path = Path.append ctx.build_dir src_path in
          let install_alias = Alias.install ~dir:path in
          let install_file = Path.relative path install_fn in
          SC.add_alias_deps sctx install_alias (Path.Set.singleton install_file))

  let init () =
    init_meta ();
    init_install ();
    init_install_files ()
end
