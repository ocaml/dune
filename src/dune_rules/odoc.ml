open Import
open Memo.O
module Gen_rules = Build_config.Gen_rules

let ( ++ ) = Path.Build.relative

let find_project_by_key =
  let memo =
    let make_map projects =
      Dune_project.File_key.Map.of_list_map_exn projects ~f:(fun project ->
        Dune_project.file_key project, project)
      |> Memo.return
    in
    let module Input = struct
      type t = Dune_project.t list

      let equal = List.equal Dune_project.equal
      let hash = List.hash Dune_project.hash
      let to_dyn = Dyn.list Dune_project.to_dyn
    end
    in
    Memo.create "project-by-keys" ~input:(module Input) make_map
  in
  fun key ->
    let* projects = Dune_load.projects () in
    let+ map = Memo.exec memo projects in
    Dune_project.File_key.Map.find_exn map key
;;

module Scope_key : sig
  val of_string : Context_name.t -> string -> (Lib_name.t * Lib.DB.t) Memo.t
  val to_string : Lib_name.t -> Dune_project.t -> string
end = struct
  let of_string context s =
    match String.rsplit2 s ~on:'@' with
    | None ->
      let+ public_libs = Scope.DB.public_libs context in
      Lib_name.parse_string_exn (Loc.none, s), public_libs
    | Some (lib, key) ->
      let+ scope =
        let key = Dune_project.File_key.of_string key in
        find_project_by_key key >>= Scope.DB.find_by_project context
      in
      Lib_name.parse_string_exn (Loc.none, lib), Scope.libs scope
  ;;

  let to_string lib project =
    let key = Dune_project.file_key project in
    sprintf "%s@%s" (Lib_name.to_string lib) (Dune_project.File_key.to_string key)
  ;;
end

let lib_unique_name lib =
  let name = Lib.name lib in
  let info = Lib.info lib in
  let status = Lib_info.status info in
  match status with
  | Installed_private | Installed -> assert false
  | Public _ -> Lib_name.to_string name
  | Private (project, _) -> Scope_key.to_string name project
;;

let pkg_or_lnu lib =
  match Lib_info.package (Lib.info lib) with
  | Some p -> Package.Name.to_string p
  | None -> lib_unique_name lib
;;

type target =
  | Lib of Lib.Local.t
  | Pkg of Package.Name.t

type odoc_artefact =
  { odoc_file : Path.Build.t
  ; odocl_file : Path.Build.t
  ; html_file : Path.Build.t
  ; json_file : Path.Build.t
  }

let add_rule sctx =
  let dir = Super_context.context sctx |> Context.build_dir in
  Super_context.add_rule sctx ~dir
;;

module Paths = struct
  let odoc_support_dirname = "odoc.support"
  let root (context : Context.t) = Path.Build.relative (Context.build_dir context) "_doc"

  let odocs ctx = function
    | Lib lib ->
      let obj_dir = Lib.Local.obj_dir lib in
      Obj_dir.odoc_dir obj_dir
    | Pkg pkg -> root ctx ++ sprintf "_odoc/pkg/%s" (Package.Name.to_string pkg)
  ;;

  let html_root ctx = root ctx ++ "_html"
  let odocl_root ctx = root ctx ++ "_odocls"

  let add_pkg_lnu base m =
    base
    ++
    match m with
    | Pkg pkg -> Package.Name.to_string pkg
    | Lib lib -> pkg_or_lnu (Lib.Local.to_lib lib)
  ;;

  let html ctx m = add_pkg_lnu (html_root ctx) m
  let odocl ctx m = add_pkg_lnu (odocl_root ctx) m
  let gen_mld_dir ctx pkg = root ctx ++ "_mlds" ++ Package.Name.to_string pkg
  let odoc_support ctx = html_root ctx ++ odoc_support_dirname
  let toplevel_index ctx = html_root ctx ++ "index.html"
end

module Output_format = struct
  type t =
    | Html
    | Json

  let all = [ Html; Json ]
  let iter ~f = Memo.parallel_iter all ~f

  let extension = function
    | Html -> ".html"
    | Json -> ".html.json"
  ;;

  let args = function
    | Html -> Command.Args.empty
    | Json -> A "--as-json"
  ;;

  let target t odoc_file =
    match t with
    | Html -> odoc_file.html_file
    | Json -> odoc_file.json_file
  ;;

  let alias t ~dir =
    match t with
    | Html -> Alias.make Alias0.doc ~dir
    | Json -> Alias.make Alias0.doc_json ~dir
  ;;

  let toplevel_index_path format ctx =
    let base = Paths.toplevel_index ctx in
    match format with
    | Html -> base
    | Json -> Path.Build.extend_basename base ~suffix:".json"
  ;;
end

module Dep : sig
  (** [format_alias output ctx target] returns the alias that depends on all
      targets produced by odoc for [target] in output format [output]. *)
  val format_alias : Output_format.t -> Context.t -> target -> Alias.t

  (** [deps ctx pkg libraries] returns all odoc dependencies of [libraries]. If
      [libraries] are all part of a package [pkg], then the odoc dependencies of
      the package are also returned*)
  val deps
    :  Context.t
    -> Package.Name.t option
    -> Lib.t list Resolve.t
    -> unit Action_builder.t

  (*** [setup_deps ctx target odocs] Adds [odocs] as dependencies for [target].
    These dependencies may be used using the [deps] function *)
  val setup_deps : Context.t -> target -> Path.Set.t -> unit Memo.t
end = struct
  let format_alias f ctx m = Output_format.alias f ~dir:(Paths.html ctx m)
  let alias = Alias.make (Alias.Name.of_string ".odoc-all")

  let deps ctx pkg requires =
    let open Action_builder.O in
    let* libs = Resolve.read requires in
    Action_builder.deps
      (let init =
         match pkg with
         | Some p -> Dep.Set.singleton (Dep.alias (alias ~dir:(Paths.odocs ctx (Pkg p))))
         | None -> Dep.Set.empty
       in
       List.fold_left libs ~init ~f:(fun acc (lib : Lib.t) ->
         match Lib.Local.of_lib lib with
         | None -> acc
         | Some lib ->
           let dir = Paths.odocs ctx (Lib lib) in
           let alias = alias ~dir in
           Dep.Set.add acc (Dep.alias alias)))
  ;;

  let alias ctx m = alias ~dir:(Paths.odocs ctx m)

  let setup_deps ctx m files =
    Rules.Produce.Alias.add_deps (alias ctx m) (Action_builder.path_set files)
  ;;
end

let odoc_ext = ".odoc"

module Mld : sig
  type t

  val create : Path.Build.t -> t
  val odoc_file : doc_dir:Path.Build.t -> t -> Path.Build.t
  val odoc_input : t -> Path.Build.t
end = struct
  type t = Path.Build.t

  let create p = p

  let odoc_file ~doc_dir t =
    let t = Filename.remove_extension (Path.Build.basename t) in
    Path.Build.relative doc_dir (sprintf "page-%s%s" t odoc_ext)
  ;;

  let odoc_input t = t
end

module Flags = struct
  type warnings = Dune_env.Odoc.warnings =
    | Fatal
    | Nonfatal

  type t = { warnings : warnings }

  let default = { warnings = Nonfatal }

  let get ~dir =
    Env_stanza_db.value ~default ~dir ~f:(fun config ->
      match config.odoc.warnings with
      | None -> Memo.return None
      | Some warnings -> Memo.return (Some { warnings }))
    |> Action_builder.of_memo
  ;;
end

let odoc_base_flags quiet build_dir =
  let open Action_builder.O in
  let+ conf = Flags.get ~dir:build_dir in
  match conf.warnings with
  | Fatal ->
    (* if quiet has been passed, we're running odoc on an external
       artifact (e.g. stdlib.cmti) - so no point in warn-error *)
    if quiet then Command.Args.S [] else A "--warn-error"
  | Nonfatal -> S []
;;

let odoc_program sctx dir =
  Super_context.resolve_program
    sctx
    ~dir
    ~where:Original_path
    "odoc"
    ~loc:None
    ~hint:"opam install odoc"
;;

let run_odoc sctx ~dir command ~quiet ~flags_for args =
  let build_dir = Super_context.context sctx |> Context.build_dir in
  let program = odoc_program sctx build_dir in
  let base_flags =
    let open Action_builder.O in
    let* () = Action_builder.return () in
    match flags_for with
    | None -> Action_builder.return Command.Args.empty
    | Some path -> odoc_base_flags quiet path
  in
  let deps = Action_builder.env_var "ODOC_SYNTAX" in
  let open Action_builder.With_targets.O in
  Action_builder.with_no_targets deps
  >>> Command.run_dyn_prog ~dir program [ A command; Dyn base_flags; S args ]
;;

let module_deps (m : Module.t) ~obj_dir ~(dep_graphs : Dep_graph.Ml_kind.t) =
  Action_builder.dyn_paths_unit
    (let open Action_builder.O in
     let+ deps =
       if Module.has m ~ml_kind:Intf
       then Dep_graph.deps_of dep_graphs.intf m
       else
         (* When a module has no .mli, use the dependencies for the .ml *)
         Dep_graph.deps_of dep_graphs.impl m
     in
     List.map deps ~f:(fun m -> Path.build (Obj_dir.Module.odoc obj_dir m)))
;;

let compile_module
  sctx
  ~obj_dir
  (m : Module.t)
  ~includes:(file_deps, iflags)
  ~dep_graphs
  ~pkg_or_lnu
  ~mode
  =
  let odoc_file = Obj_dir.Module.odoc obj_dir m in
  let open Memo.O in
  let+ () =
    let action_with_targets =
      let doc_dir = Path.build (Obj_dir.odoc_dir obj_dir) in
      let run_odoc =
        run_odoc
          sctx
          ~dir:doc_dir
          "compile"
          ~quiet:false
          ~flags_for:(Some odoc_file)
          [ A "-I"
          ; Path doc_dir
          ; iflags
          ; As [ "--pkg"; pkg_or_lnu ]
          ; A "-o"
          ; Target odoc_file
          ; Dep
              (Path.build
                 (Obj_dir.Module.cmti_file
                    ~cm_kind:
                      (match mode with
                       | Lib_mode.Ocaml _ -> Ocaml Cmi
                       | Melange -> Melange Cmi)
                    obj_dir
                    m))
          ]
      in
      let open Action_builder.With_targets.O in
      Action_builder.with_no_targets file_deps
      >>> Action_builder.with_no_targets (module_deps m ~obj_dir ~dep_graphs)
      >>> run_odoc
    in
    add_rule sctx action_with_targets
  in
  m, odoc_file
;;

let compile_mld sctx (m : Mld.t) ~includes ~doc_dir ~pkg =
  let open Memo.O in
  let odoc_file = Mld.odoc_file m ~doc_dir in
  let odoc_input = Mld.odoc_input m in
  let run_odoc =
    run_odoc
      sctx
      ~dir:(Path.build doc_dir)
      "compile"
      ~quiet:false
      ~flags_for:(Some odoc_input)
      [ Command.Args.dyn includes
      ; As [ "--pkg"; Package.Name.to_string pkg ]
      ; A "-o"
      ; Target odoc_file
      ; Dep (Path.build odoc_input)
      ]
  in
  let+ () = add_rule sctx run_odoc in
  odoc_file
;;

let odoc_include_flags ctx pkg requires =
  Resolve.args
    (let open Resolve.O in
     let+ libs = requires in
     let paths =
       List.fold_left libs ~init:Path.Set.empty ~f:(fun paths lib ->
         match Lib.Local.of_lib lib with
         | None -> paths
         | Some lib -> Path.Set.add paths (Path.build (Paths.odocs ctx (Lib lib))))
     in
     let paths =
       match pkg with
       | Some p -> Path.Set.add paths (Path.build (Paths.odocs ctx (Pkg p)))
       | None -> paths
     in
     Command.Args.S
       (List.concat_map (Path.Set.to_list paths) ~f:(fun dir ->
          [ Command.Args.A "-I"; Path dir ])))
;;

let link_odoc_rules sctx (odoc_file : odoc_artefact) ~pkg ~requires =
  let ctx = Super_context.context sctx in
  let deps = Dep.deps ctx pkg requires in
  let run_odoc =
    run_odoc
      sctx
      ~dir:(Path.build (Paths.html_root ctx))
      "link"
      ~quiet:false
      ~flags_for:(Some odoc_file.odoc_file)
      [ odoc_include_flags ctx pkg requires
      ; A "-o"
      ; Target odoc_file.odocl_file
      ; Dep (Path.build odoc_file.odoc_file)
      ]
  in
  add_rule
    sctx
    (let open Action_builder.With_targets.O in
     Action_builder.with_no_targets deps >>> run_odoc)
;;

let setup_library_odoc_rules cctx (local_lib : Lib.Local.t) =
  (* Using the proper package name doesn't actually work since odoc assumes that
     a package contains only 1 library *)
  let pkg_or_lnu = pkg_or_lnu (Lib.Local.to_lib local_lib) in
  let sctx = Compilation_context.super_context cctx in
  let ctx = Super_context.context sctx in
  let info = Lib.Local.info local_lib in
  let obj_dir = Compilation_context.obj_dir cctx in
  let modules = Compilation_context.modules cctx in
  let* includes =
    let+ requires = Compilation_context.requires_compile cctx in
    let package = Lib_info.package info in
    let odoc_include_flags =
      Command.Args.memo (odoc_include_flags ctx package requires)
    in
    Dep.deps ctx package requires, odoc_include_flags
  in
  modules
  |> Modules.With_vlib.drop_vlib
  |> Modules.fold ~init:[] ~f:(fun m acc ->
    let compiled =
      let modes = Lib_info.modes info in
      let mode = Lib_mode.Map.Set.for_merlin modes in
      compile_module
        sctx
        ~includes
        ~dep_graphs:(Compilation_context.dep_graphs cctx)
        ~obj_dir
        ~pkg_or_lnu
        ~mode
        m
    in
    compiled :: acc)
  |> Memo.all_concurrently
  >>| Path.Set.of_list_map ~f:(fun (_, p) -> Path.build p)
  >>= Dep.setup_deps ctx (Lib local_lib)
;;

let setup_generate sctx ~search_db odoc_file out =
  let ctx = Super_context.context sctx in
  let odoc_support_path = Paths.odoc_support ctx in
  let search_args =
    Sherlodoc.odoc_args sctx ~search_db ~dir_sherlodoc_dot_js:(Paths.html_root ctx)
  in
  let run_odoc =
    run_odoc
      sctx
      ~dir:(Path.build (Paths.html_root ctx))
      "html-generate"
      ~quiet:false
      ~flags_for:None
      [ search_args
      ; A "-o"
      ; Path (Path.build (Paths.html_root ctx))
      ; A "--support-uri"
      ; Path (Path.build odoc_support_path)
      ; A "--theme-uri"
      ; Path (Path.build odoc_support_path)
      ; Dep (Path.build odoc_file.odocl_file)
      ; Output_format.args out
      ; Hidden_targets [ Output_format.target out odoc_file ]
      ]
  in
  add_rule sctx run_odoc
;;

let setup_generate_all sctx ~search_db odoc_file =
  Output_format.iter ~f:(setup_generate sctx ~search_db odoc_file)
;;

let setup_css_rule sctx =
  let ctx = Super_context.context sctx in
  let dir = Paths.odoc_support ctx in
  let run_odoc =
    let cmd =
      run_odoc
        sctx
        ~dir:(Path.build (Context.build_dir ctx))
        "support-files"
        ~quiet:false
        ~flags_for:None
        [ A "-o"; Path (Path.build dir) ]
    in
    Action_builder.With_targets.add_directories ~directory_targets:[ dir ] cmd
  in
  add_rule sctx run_odoc
;;

let sp = Printf.sprintf

module Toplevel_index = struct
  type item =
    { name : string
    ; version : Package_version.t option
    ; link : string
    }

  let of_packages packages =
    Package.Name.Map.to_list_map packages ~f:(fun name package ->
      let name = Package.Name.to_string name in
      { name; version = Package.version package; link = sp "%s/index.html" name })
  ;;

  let html_list_items t =
    List.map t ~f:(fun { name; version; link } ->
      let link = sp {|<a href="%s">%s</a>|} link name in
      let version_suffix =
        match version with
        | None -> ""
        | Some v -> sp {| <span class="version">%s</span>|} (Package_version.to_string v)
      in
      sp "<li>%s%s</li>" link version_suffix)
    |> String.concat ~sep:"\n      "
  ;;

  let html t =
    sp
      {|<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
    <title>index</title>
    <link rel="stylesheet" href="./%s/odoc.css"/>
    <meta charset="utf-8"/>
    <meta name="viewport" content="width=device-width,initial-scale=1.0"/>
  </head>
  <body>
    <main class="content">
      <div class="by-name">
      <h2>OCaml package documentation</h2>
      <ol>
      %s
      </ol>
      </div>
    </main>
  </body>
</html>|}
      Paths.odoc_support_dirname
      (html_list_items t)
  ;;

  let string_to_json s = `String s
  let list_to_json ~f l = `List (List.map ~f l)

  let option_to_json ~f = function
    | None -> `Null
    | Some x -> f x
  ;;

  let item_to_json { name; version; link } =
    `Assoc
      [ "name", string_to_json name
      ; ( "version"
        , Option.map ~f:Package_version.to_string version
          |> option_to_json ~f:string_to_json )
      ; "link", string_to_json link
      ]
  ;;

  (** This format is public API. *)
  let to_json items = `Assoc [ "packages", list_to_json items ~f:item_to_json ]

  let json t = Dune_stats.Json.to_string (to_json t)

  let content (output : Output_format.t) t =
    match output with
    | Html -> html t
    | Json -> json t
  ;;
end

let setup_toplevel_index_rule sctx output =
  let* packages = Dune_load.packages () in
  let index = Toplevel_index.of_packages packages in
  let content = Toplevel_index.content output index in
  let ctx = Super_context.context sctx in
  let path = Output_format.toplevel_index_path output ctx in
  add_rule sctx (Action_builder.write_file path content)
;;

let setup_toplevel_index_rules sctx =
  Output_format.iter ~f:(setup_toplevel_index_rule sctx)
;;

let libs_of_pkg ctx ~pkg =
  let+ entries = Scope.DB.lib_entries_of_package ctx pkg in
  (* Filter out all implementations of virtual libraries *)
  List.filter_map entries ~f:(fun (entry : Scope.DB.Lib_entry.t) ->
    match entry with
    | Deprecated_library_name _ -> None
    | Library lib ->
      (match Lib.Local.to_lib lib |> Lib.info |> Lib_info.implements with
       | None -> Some lib
       | Some _ -> None))
;;

let entry_modules_by_lib sctx lib =
  Dir_contents.modules_of_local_lib sctx lib
  >>| Modules.With_vlib.modules
  >>| Modules.With_vlib.entry_modules
;;

let entry_modules sctx ~pkg =
  let* l =
    Super_context.context sctx
    |> Context.name
    |> libs_of_pkg ~pkg
    >>| List.filter ~f:(fun lib ->
      Lib.Local.info lib |> Lib_info.status |> Lib_info.Status.is_private |> not)
  in
  let+ l =
    Memo.parallel_map l ~f:(fun l ->
      let+ m = entry_modules_by_lib sctx l in
      l, m)
  in
  Lib.Local.Map.of_list_exn l
;;

let create_odoc ctx ~target odoc_file =
  let html_base = Paths.html ctx target in
  let odocl_base = Paths.odocl ctx target in
  let basename = Path.Build.basename odoc_file |> Filename.remove_extension in
  let odocl_file = odocl_base ++ (basename ^ ".odocl") in
  match target with
  | Lib _ ->
    let html_dir = html_base ++ Stdune.String.capitalize basename in
    let file output =
      html_dir ++ "index"
      |> Path.Build.extend_basename ~suffix:(Output_format.extension output)
    in
    { odoc_file; odocl_file; html_file = file Html; json_file = file Json }
  | Pkg _ ->
    let file output =
      html_base ++ (basename |> String.drop_prefix ~prefix:"page-" |> Option.value_exn)
      |> Path.Build.extend_basename ~suffix:(Output_format.extension output)
    in
    { odoc_file; odocl_file; html_file = file Html; json_file = file Json }
;;

let check_mlds_no_dupes ~pkg ~mlds =
  match
    List.rev_map mlds ~f:(fun mld ->
      Filename.remove_extension (Path.Build.basename mld), mld)
    |> Filename.Map.of_list
  with
  | Ok m -> m
  | Error (_, p1, p2) ->
    User_error.raise
      [ Pp.textf
          "Package %s has two mld's with the same basename %s, %s"
          (Package.Name.to_string pkg)
          (Path.to_string_maybe_quoted (Path.build p1))
          (Path.to_string_maybe_quoted (Path.build p2))
      ]
;;

let odoc_artefacts sctx target =
  let ctx = Super_context.context sctx in
  let dir = Paths.odocs ctx target in
  match target with
  | Pkg pkg ->
    let+ mlds =
      let+ mlds = Packages.mlds sctx pkg in
      let mlds = check_mlds_no_dupes ~pkg ~mlds in
      Filename.Map.update mlds "index" ~f:(function
        | None -> Some (Paths.gen_mld_dir ctx pkg ++ "index.mld")
        | Some _ as s -> s)
    in
    Filename.Map.to_list_map mlds ~f:(fun _ mld ->
      Mld.create mld |> Mld.odoc_file ~doc_dir:dir |> create_odoc ctx ~target)
  | Lib lib ->
    let info = Lib.Local.info lib in
    let obj_dir = Lib_info.obj_dir info in
    let+ modules = entry_modules_by_lib sctx lib in
    List.map modules ~f:(fun m ->
      let odoc_file = Obj_dir.Module.odoc obj_dir m in
      create_odoc ctx ~target odoc_file)
;;

let setup_lib_odocl_rules_def =
  let module Input = struct
    module Super_context = Super_context.As_memo_key

    type t = Super_context.t * Lib.Local.t * Lib.t list Resolve.t

    let equal (sc1, l1, r1) (sc2, l2, r2) =
      Super_context.equal sc1 sc2
      && Lib.Local.equal l1 l2
      && Resolve.equal (List.equal Lib.equal) r1 r2
    ;;

    let hash (sc, l, r) =
      Poly.hash
        (Super_context.hash sc, Lib.Local.hash l, Resolve.hash (List.hash Lib.hash) r)
    ;;

    let to_dyn _ = Dyn.Opaque
  end
  in
  let f (sctx, lib, requires) =
    let* odocs = odoc_artefacts sctx (Lib lib) in
    let pkg = Lib_info.package (Lib.Local.info lib) in
    Memo.parallel_iter odocs ~f:(fun odoc -> link_odoc_rules sctx ~pkg ~requires odoc)
  in
  Memo.With_implicit_output.create
    "setup_library_odocls_rules"
    ~implicit_output:Rules.implicit_output
    ~input:(module Input)
    f
;;

let setup_lib_odocl_rules sctx lib ~requires =
  Memo.With_implicit_output.exec setup_lib_odocl_rules_def (sctx, lib, requires)
;;

let setup_pkg_rules_def memo_name f =
  let module Input = struct
    module Super_context = Super_context.As_memo_key

    type t = Super_context.t * Package.Name.t

    let equal (s1, p1) (s2, p2) = Package.Name.equal p1 p2 && Super_context.equal s1 s2
    let hash = Tuple.T2.hash Super_context.hash Package.Name.hash
    let to_dyn (_, package) = Package.Name.to_dyn package
  end
  in
  Memo.With_implicit_output.create
    memo_name
    ~input:(module Input)
    ~implicit_output:Rules.implicit_output
    f
;;

let setup_pkg_odocl_rules_def =
  let f (sctx, pkg) =
    let* libs = Super_context.context sctx |> Context.name |> libs_of_pkg ~pkg in
    let* requires =
      let libs = (libs :> Lib.t list) in
      Lib.closure libs ~linking:false
    in
    let* () = Memo.parallel_iter libs ~f:(setup_lib_odocl_rules sctx ~requires)
    and* _ =
      let* pkg_odocs = odoc_artefacts sctx (Pkg pkg) in
      let pkg = Some pkg in
      let+ () =
        Memo.parallel_iter pkg_odocs ~f:(fun odoc ->
          link_odoc_rules sctx ~pkg ~requires odoc)
      in
      pkg_odocs
    and* _ = Memo.parallel_map libs ~f:(fun lib -> odoc_artefacts sctx (Lib lib)) in
    Memo.return ()
  in
  setup_pkg_rules_def "setup-package-odocls-rules" f
;;

let setup_pkg_odocl_rules sctx ~pkg : unit Memo.t =
  Memo.With_implicit_output.exec setup_pkg_odocl_rules_def (sctx, pkg)
;;

let out_file (output : Output_format.t) odoc =
  match output with
  | Html -> odoc.html_file
  | Json -> odoc.json_file
;;

let out_files ctx (output : Output_format.t) odocs =
  let extra_files =
    match output with
    | Html -> [ Path.build (Paths.odoc_support ctx) ]
    | Json -> []
  in
  Path.build (Output_format.toplevel_index_path output ctx)
  :: List.rev_append
       extra_files
       (List.map odocs ~f:(fun odoc -> Path.build (out_file output odoc)))
;;

let setup_lib_html_rules_def =
  let module Input = struct
    module Super_context = Super_context.As_memo_key

    type t = Super_context.t * Lib.Local.t

    let equal (sc1, l1) (sc2, l2) = Super_context.equal sc1 sc2 && Lib.Local.equal l1 l2
    let hash = Tuple.T2.hash Super_context.hash Lib.Local.hash
    let to_dyn _ = Dyn.Opaque
  end
  in
  let f (sctx, lib) =
    let ctx = Super_context.context sctx in
    let target = Lib lib in
    let* odocs = odoc_artefacts sctx target in
    Output_format.iter ~f:(fun output ->
      let paths = out_files ctx output odocs in
      Rules.Produce.Alias.add_deps
        (Dep.format_alias output ctx target)
        (Action_builder.paths paths))
  in
  Memo.With_implicit_output.create
    "setup-library-html-rules"
    ~implicit_output:Rules.implicit_output
    ~input:(module Input)
    f
;;

let search_db_for_lib sctx lib =
  let target = Lib lib in
  let ctx = Super_context.context sctx in
  let dir = Paths.html ctx target in
  let* odocs = odoc_artefacts sctx target in
  let odocls = List.map odocs ~f:(fun odoc -> odoc.odocl_file) in
  Sherlodoc.search_db sctx ~dir ~external_odocls:[] odocls
;;

let setup_lib_html_rules sctx ~search_db lib =
  let target = Lib lib in
  let* odocs = odoc_artefacts sctx target in
  let* () =
    Memo.parallel_iter odocs ~f:(fun odoc -> setup_generate_all sctx ~search_db odoc)
  in
  Memo.With_implicit_output.exec setup_lib_html_rules_def (sctx, lib)
;;

let setup_pkg_html_rules_def =
  let f (sctx, pkg) =
    let ctx = Super_context.context sctx in
    let* libs = Context.name ctx |> libs_of_pkg ~pkg in
    let dir = Paths.html ctx (Pkg pkg) in
    let* pkg_odocs = odoc_artefacts sctx (Pkg pkg) in
    let* lib_odocs =
      Memo.List.concat_map libs ~f:(fun lib -> odoc_artefacts sctx (Lib lib))
    in
    let all_odocs = pkg_odocs @ lib_odocs in
    let* search_db =
      let odocls = List.map all_odocs ~f:(fun artefact -> artefact.odocl_file) in
      Sherlodoc.search_db sctx ~dir ~external_odocls:[] odocls
    in
    let* () = Memo.parallel_iter libs ~f:(setup_lib_html_rules sctx ~search_db) in
    let* () = Memo.parallel_iter pkg_odocs ~f:(setup_generate_all ~search_db sctx) in
    Output_format.iter ~f:(fun output ->
      let paths = out_files ctx output all_odocs in
      Rules.Produce.Alias.add_deps
        (Dep.format_alias output ctx (Pkg pkg))
        (Action_builder.paths paths))
  in
  setup_pkg_rules_def "setup-package-html-rules" f
;;

let setup_pkg_html_rules sctx ~pkg : unit Memo.t =
  Memo.With_implicit_output.exec setup_pkg_html_rules_def (sctx, pkg)
;;

let setup_package_aliases_format sctx (pkg : Package.t) (output : Output_format.t) =
  let ctx = Super_context.context sctx in
  let name = Package.name pkg in
  let alias =
    let pkg_dir = Package.dir pkg in
    let dir = Path.Build.append_source (Context.build_dir ctx) pkg_dir in
    Output_format.alias output ~dir
  in
  let* libs =
    Context.name ctx |> libs_of_pkg ~pkg:name >>| List.map ~f:(fun lib -> Lib lib)
  in
  Pkg name :: libs
  |> List.map ~f:(Dep.format_alias output ctx)
  |> Dune_engine.Dep.Set.of_list_map ~f:(fun f -> Dune_engine.Dep.alias f)
  |> Action_builder.deps
  |> Rules.Produce.Alias.add_deps alias
;;

let setup_package_aliases sctx (pkg : Package.t) =
  Output_format.iter ~f:(setup_package_aliases_format sctx pkg)
;;

let default_index ~pkg entry_modules =
  let b = Buffer.create 512 in
  Printf.bprintf b "{0 %s index}\n" (Package.Name.to_string pkg);
  Lib.Local.Map.to_list entry_modules
  |> List.sort ~compare:(fun (x, _) (y, _) ->
    let name lib = Lib.name (Lib.Local.to_lib lib) in
    Lib_name.compare (name x) (name y))
  |> List.iter ~f:(fun (lib, modules) ->
    let lib = Lib.Local.to_lib lib in
    Printf.bprintf b "{1 Library %s}\n" (Lib_name.to_string (Lib.name lib));
    Buffer.add_string
      b
      (match modules with
       | [ x ] ->
         sprintf
           "The entry point of this library is the module:\n{!module-%s}.\n"
           (Module_name.to_string (Module.name x))
       | _ ->
         sprintf
           "This library exposes the following toplevel modules:\n{!modules:%s}\n"
           (modules
            |> List.filter ~f:(fun m -> Module.visibility m = Visibility.Public)
            |> List.sort ~compare:(fun x y ->
              Module_name.compare (Module.name x) (Module.name y))
            |> List.map ~f:(fun m -> Module_name.to_string (Module.name m))
            |> String.concat ~sep:" ")));
  Buffer.contents b
;;

let package_mlds =
  let memo =
    Memo.create
      "package-mlds"
      ~input:(module Super_context.As_memo_key.And_package_name)
      (fun (sctx, pkg) ->
        Rules.collect (fun () ->
          let* mlds = Packages.mlds sctx pkg in
          let mlds = check_mlds_no_dupes ~pkg ~mlds in
          let ctx = Super_context.context sctx in
          if Filename.Map.mem mlds "index"
          then Memo.return mlds
          else (
            let gen_mld = Paths.gen_mld_dir ctx pkg ++ "index.mld" in
            let* entry_modules = entry_modules sctx ~pkg in
            let+ () =
              add_rule
                sctx
                (Action_builder.write_file gen_mld (default_index ~pkg entry_modules))
            in
            Filename.Map.set mlds "index" gen_mld)))
  in
  fun sctx ~pkg -> Memo.exec memo (sctx, pkg)
;;

let setup_package_odoc_rules sctx ~pkg =
  let* mlds = package_mlds sctx ~pkg >>| fst in
  let ctx = Super_context.context sctx in
  (* CR-someday jeremiedimino: it is weird that we drop the [Package.t] and go
     back to a package name here. Need to try and change that one day. *)
  let* odocs =
    Filename.Map.values mlds
    |> Memo.parallel_map ~f:(fun mld ->
      compile_mld
        sctx
        (Mld.create mld)
        ~pkg
        ~doc_dir:(Paths.odocs ctx (Pkg pkg))
        ~includes:(Action_builder.return []))
  in
  Dep.setup_deps ctx (Pkg pkg) (Path.set_of_build_paths_list odocs)
;;

let gen_project_rules sctx project =
  Dune_project.packages project
  |> Dune_lang.Package_name.Map.to_seq
  |> Memo.parallel_iter_seq ~f:(fun (_, (pkg : Package.t)) ->
    (* setup @doc to build the correct html for the package *)
    setup_package_aliases sctx pkg)
;;

let setup_private_library_doc_alias sctx ~scope ~dir (l : Library.t) =
  match l.visibility with
  | Public _ -> Memo.return ()
  | Private _ ->
    let ctx = Super_context.context sctx in
    let* lib =
      let src_dir = Path.drop_optional_build_context_src_exn (Path.build dir) in
      Lib.DB.find_lib_id_even_when_hidden
        (Scope.libs scope)
        (Local (Library.to_lib_id ~src_dir l))
      >>| Option.value_exn
    in
    let lib = Lib (Lib.Local.of_lib_exn lib) in
    Rules.Produce.Alias.add_deps
      (Alias.make ~dir Alias0.private_doc)
      (lib |> Dep.format_alias Html ctx |> Dune_engine.Dep.alias |> Action_builder.dep)
;;

let has_rules ?(directory_targets = Path.Build.Map.empty) m =
  let rules = Rules.collect_unit (fun () -> m) in
  Memo.return (Gen_rules.make ~directory_targets rules)
;;

let with_package pkg ~f =
  let pkg = Package.Name.of_string pkg in
  let* packages = Dune_load.packages () in
  match Package.Name.Map.find packages pkg with
  | Some pkg -> has_rules (f pkg)
  | None -> Memo.return Gen_rules.no_rules
;;

let gen_rules sctx ~dir rest =
  match rest with
  | [] ->
    Memo.return
      (Build_config.Gen_rules.make
         ~build_dir_only_sub_dirs:
           (Build_config.Gen_rules.Build_only_sub_dirs.singleton ~dir Subdir_set.all)
         (Memo.return Rules.empty))
  | [ "_html" ] ->
    let ctx = Super_context.context sctx in
    let directory_targets = Path.Build.Map.singleton (Paths.odoc_support ctx) Loc.none in
    has_rules
      ~directory_targets
      (Sherlodoc.sherlodoc_dot_js sctx ~dir:(Paths.html_root ctx)
       >>> setup_css_rule sctx
       >>> setup_toplevel_index_rules sctx)
  | [ "_mlds"; pkg ] ->
    with_package pkg ~f:(fun pkg ->
      let pkg = Package.name pkg in
      let* _mlds, rules = package_mlds sctx ~pkg in
      Rules.produce rules)
  | [ "_odoc"; "pkg"; pkg ] ->
    with_package pkg ~f:(fun pkg ->
      let pkg = Package.name pkg in
      setup_package_odoc_rules sctx ~pkg)
  | [ "_odocls"; lib_unique_name_or_pkg ] ->
    has_rules
      ((* TODO we can be a better with the error handling in the case where
          lib_unique_name_or_pkg is neither a valid pkg or lnu *)
       let ctx = Super_context.context sctx in
       let* lib, lib_db = Scope_key.of_string (Context.name ctx) lib_unique_name_or_pkg in
       (* jeremiedimino: why isn't [None] some kind of error here? *)
       let* lib =
         let+ lib = Lib.DB.find lib_db lib in
         Option.bind ~f:Lib.Local.of_lib lib
       in
       let+ () =
         match lib with
         | None -> Memo.return ()
         | Some lib ->
           (match Lib_info.package (Lib.Local.info lib) with
            | None ->
              let* requires = Lib.closure [ Lib.Local.to_lib lib ] ~linking:false in
              setup_lib_odocl_rules sctx lib ~requires
            | Some pkg -> setup_pkg_odocl_rules sctx ~pkg)
       and+ () =
         let* packages = Dune_load.packages () in
         match
           Package.Name.Map.find packages (Package.Name.of_string lib_unique_name_or_pkg)
         with
         | None -> Memo.return ()
         | Some pkg ->
           let name = Package.name pkg in
           setup_pkg_odocl_rules sctx ~pkg:name
       in
       ())
  | [ "_html"; lib_unique_name_or_pkg ] ->
    has_rules
      ((* TODO we can be a better with the error handling in the case where
          lib_unique_name_or_pkg is neither a valid pkg or lnu *)
       let ctx = Super_context.context sctx in
       let* lib, lib_db = Scope_key.of_string (Context.name ctx) lib_unique_name_or_pkg in
       (* jeremiedimino: why isn't [None] some kind of error here? *)
       let* lib =
         let+ lib = Lib.DB.find lib_db lib in
         Option.bind ~f:Lib.Local.of_lib lib
       in
       let+ () =
         match lib with
         | None -> Memo.return ()
         | Some lib ->
           (match Lib_info.package (Lib.Local.info lib) with
            | None ->
              (* lib with no package above it *)
              let* search_db = search_db_for_lib sctx lib in
              setup_lib_html_rules sctx ~search_db lib
            | Some pkg -> setup_pkg_html_rules sctx ~pkg)
       and+ () =
         let* packages = Dune_load.packages () in
         match
           Package.Name.Map.find packages (Package.Name.of_string lib_unique_name_or_pkg)
         with
         | None -> Memo.return ()
         | Some pkg ->
           let name = Package.name pkg in
           setup_pkg_html_rules sctx ~pkg:name
       in
       ())
  | _ -> Memo.return (Gen_rules.redirect_to_parent Gen_rules.Rules.empty)
;;
