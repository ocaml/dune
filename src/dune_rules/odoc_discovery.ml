(** Artifact discovery for odoc packages and libraries *)

open Import
open Memo.O

let ( ++ ) = Path.Build.relative
let sp = Printf.sprintf

(* Get list of local workspace packages, respecting -p flag mask *)
let get_workspace_packages () =
  let* packages = Dune_load.packages () in
  let* mask = Dune_load.mask () in
  let all_pkgs = Package.Name.Map.keys packages in
  match Only_packages.enumerate mask with
  | `All -> Memo.return all_pkgs
  | `Set visible_pkgs ->
    (* In -p mode, only return packages that are in the mask *)
    Memo.return (List.filter all_pkgs ~f:(Package_name.Set.mem visible_pkgs))
;;

let is_local_package pkg =
  let+ packages = Dune_load.packages () in
  Package.Name.Map.mem packages pkg
;;

let find_local_package pkg =
  let+ packages = Dune_load.packages () in
  Package.Name.Map.find packages pkg
;;

let stdlib_lib ctx =
  let* public_libs = Scope.DB.public_libs ctx in
  Lib.DB.find public_libs (Lib_name.of_string "stdlib")
;;

(* Get all private libraries (local libraries without a package) in the workspace.
   In -p mode, returns empty list since private libraries don't belong to packages. *)
let get_private_libraries ctx =
  let* mask = Dune_load.mask () in
  match Only_packages.enumerate mask with
  | `Set _ ->
    (* In -p mode, don't include private libraries *)
    Memo.return []
  | `All ->
    let* projects = Dune_load.projects () in
    let* find_scope_fn = Scope.DB.with_all ctx ~f:(fun find_scope -> find_scope) in
    Memo.List.concat_map projects ~f:(fun project ->
      let scope = find_scope_fn project in
      let lib_db = Scope.libs scope in
      (* Get all libraries from this scope (non-recursive to avoid duplicates from parent) *)
      let+ lib_set = Lib.DB.all ~recursive:false lib_db in
      Lib.Set.to_list lib_set
      |> List.filter_map ~f:(fun lib ->
        (* Only keep local libraries without a package *)
        match Lib.Local.of_lib lib with
        | None -> None
        | Some local_lib ->
          let info = Lib.Local.info local_lib in
          (match Lib_info.package info with
           | Some _ -> None (* Has a package, skip *)
           | None -> Some local_lib)))
;;

let libs_of_pkg (ctx : Context.t) ~pkg =
  let* packages = Dune_load.packages () in
  if Package.Name.Map.mem packages pkg
  then
    (* Local package - get libraries from Scope.DB *)
    let+ { Scope.DB.Lib_entry.Set.libraries; _ } =
      Scope.DB.lib_entries_of_package (Context.name ctx) pkg
    in
    (* Filter out implementations of virtual libraries and convert to Lib.t *)
    List.filter_map libraries ~f:(fun lib ->
      let lib_t = Lib.Local.to_lib lib in
      match Lib.info lib_t |> Lib_info.implements with
      | None -> Some lib_t
      | Some _ -> None)
  else
    (* Installed package - use Package_discovery *)
    let* pkg_discovery = Package_discovery.create ~context:ctx in
    Memo.return (Package_discovery.libraries_of_package pkg_discovery pkg)
;;

(* Get odoc config dependencies for a package, handling local vs installed.
   Local packages use (documentation (depends ...)) which only specifies packages.
   Installed packages have full odoc-config.sexp with both packages and libraries. *)
let get_odoc_config_deps_for_pkg pkg_discovery pkg =
  let* local_pkg_opt = find_local_package pkg in
  match local_pkg_opt with
  | Some local_pkg ->
    let doc = Package.info local_pkg |> Package_info.documentation in
    Memo.return
      { Odoc_config.packages =
          List.map doc.packages ~f:(fun (dep : Package_dependency.t) -> dep.name)
      ; libraries = []
      }
  | None ->
    let odoc_config = Package_discovery.config_of_package pkg_discovery pkg in
    Memo.return odoc_config.Odoc_config.deps
;;

let resolve_odoc_config_libraries lib_db ~deps =
  Memo.List.filter_map deps.Odoc_config.libraries ~f:(fun lib_name ->
    Lib.DB.find lib_db lib_name)
;;

let resolve_odoc_config_deps ctx ~deps =
  let* lib_db = Lib.DB.installed ctx in
  (* Resolve extra_libs from deps.libraries *)
  let* extra_libs_from_names = resolve_odoc_config_libraries lib_db ~deps in
  (* Resolve extra_libs from deps.packages *)
  let* extra_libs_from_pkgs =
    Memo.List.concat_map deps.Odoc_config.packages ~f:(fun pkg_name ->
      libs_of_pkg ctx ~pkg:pkg_name)
  in
  let extra_libs = extra_libs_from_names @ extra_libs_from_pkgs in
  Memo.return (extra_libs, deps.Odoc_config.packages)
;;

let resolve_pkg_odoc_config ctx ~pkg_discovery ~pkg =
  let* deps = get_odoc_config_deps_for_pkg pkg_discovery pkg in
  resolve_odoc_config_deps ctx ~deps
;;

(* Expand a set of packages with their odoc-config dependencies transitively.
   Takes initial packages and private libraries (libraries without packages).
   Returns the expanded set of packages.

   Algorithm:
   1. Get all libraries from the packages
   2. Get extra libraries from odoc-config for each package
   3. Union with private libraries
   4. Get transitive closure of library dependencies
   5. Find packages for all those libraries
   6. If there are new packages, repeat until fixed point *)
let expand_packages_with_odoc_config ctx ~packages ~private_libs =
  let* pkg_discovery = Package_discovery.create ~context:ctx in
  (* Use public_libs which includes both local and installed libs, preferring local *)
  let* lib_db = Scope.DB.public_libs (Context.name ctx) in
  (* Get stdlib once - it's implicitly required by all OCaml code *)
  let* stdlib_opt = stdlib_lib (Context.name ctx) in
  let rec expand_until_fixpoint seen_pkgs =
    (* Get all libraries from current packages *)
    let* pkg_libs =
      Package.Name.Set.to_list seen_pkgs
      |> Memo.List.concat_map ~f:(fun pkg -> libs_of_pkg ctx ~pkg)
    in
    (* Get extra libraries from odoc-config for each package *)
    let* odoc_config_libs =
      Package.Name.Set.to_list seen_pkgs
      |> Memo.List.concat_map ~f:(fun pkg ->
        let* deps = get_odoc_config_deps_for_pkg pkg_discovery pkg in
        resolve_odoc_config_libraries lib_db ~deps)
    in
    (* Union all libraries: package libs + odoc-config libs + private libs + stdlib *)
    let all_libs =
      pkg_libs @ odoc_config_libs @ private_libs @ Option.to_list stdlib_opt
    in
    (* Get transitive closure of library dependencies.
       We use descriptive_closure rather than closure because we may have
       conflicting implementations of virtual libraries when documenting
       multiple packages together - that's fine for documentation purposes. *)
    let* lib_closure = Lib.descriptive_closure all_libs ~with_pps:false in
    let* pkgs_from_libs =
      Memo.List.filter_map lib_closure ~f:(fun lib ->
        match Lib.Local.of_lib lib with
        | Some _ -> Memo.return (Lib_info.package (Lib.info lib))
        | None -> Memo.return (Package_discovery.package_of_library pkg_discovery lib))
    in
    let* odoc_config_pkgs =
      Package.Name.Set.to_list seen_pkgs
      |> Memo.List.concat_map ~f:(fun pkg ->
        let* deps = get_odoc_config_deps_for_pkg pkg_discovery pkg in
        Memo.return deps.packages)
    in
    let all_new_pkgs =
      Package.Name.Set.union
        (Package.Name.Set.of_list pkgs_from_libs)
        (Package.Name.Set.of_list odoc_config_pkgs)
    in
    let new_pkgs = Package.Name.Set.diff all_new_pkgs seen_pkgs in
    if Package.Name.Set.is_empty new_pkgs
    then Memo.return seen_pkgs
    else expand_until_fixpoint (Package.Name.Set.union seen_pkgs new_pkgs)
  in
  expand_until_fixpoint (Package.Name.Set.of_list packages)
;;

module Toplevel_index = struct
  type pkg_item =
    { name : string
    ; version : Package_version.t option
    }

  type private_lib_item =
    { unique_name : string
    ; display_name : string
    }

  type item =
    | Package of pkg_item
    | Private_lib of private_lib_item [@warning "-37"]

  let of_packages packages =
    Package.Name.Map.to_list_map packages ~f:(fun name package ->
      let name = Package.Name.to_string name in
      Package { name; version = Package.version package })
  ;;

  let mld_content t =
    let b = Buffer.create 1024 in
    Printf.bprintf b "{0 OCaml package documentation}\n\n";
    (* Separate packages and private libs *)
    let packages, private_libs =
      List.partition_map t ~f:(fun item ->
        match item with
        | Package p -> Left p
        | Private_lib p -> Right p)
    in
    if not (List.is_empty packages)
    then
      List.iter packages ~f:(fun { name; version } ->
        let version_suffix =
          match version with
          | None -> ""
          | Some v -> sp " (%s)" (Package_version.to_string v)
        in
        Printf.bprintf b "- {{!/%s/page-index}%s}%s\n" name name version_suffix);
    if not (List.is_empty private_libs)
    then (
      Printf.bprintf b "\n{1 Private libraries}\n\n";
      List.iter private_libs ~f:(fun { unique_name; display_name } ->
        Printf.bprintf b "- {{!/%s/page-index}%s}\n" unique_name display_name));
    Buffer.contents b
  ;;

  let get_full_mode_items ctx =
    let* local_packages = Dune_load.packages () in
    let local_pkg_names = Package.Name.Map.keys local_packages in
    let* private_local_libs = get_private_libraries ctx in
    let private_libs = List.map private_local_libs ~f:Lib.Local.to_lib in
    let* all_packages =
      expand_packages_with_odoc_config ctx ~packages:local_pkg_names ~private_libs
    in
    let* pkg_discovery = Package_discovery.create ~context:ctx in
    let* pkg_items =
      Memo.List.map (Package.Name.Set.to_list all_packages) ~f:(fun pkg ->
        match Package.Name.Map.find local_packages pkg with
        | Some local_pkg ->
          Memo.return
            (Package
               { name = Package.Name.to_string pkg; version = Package.version local_pkg })
        | None ->
          let+ version = Package_discovery.version_of_package pkg_discovery pkg in
          Package
            { name = Package.Name.to_string pkg
            ; version = Option.map version ~f:Package_version.of_string
            })
    in
    let private_lib_items =
      List.map private_local_libs ~f:(fun local_lib ->
        let unique_name = Odoc_scope.lib_unique_name local_lib in
        let display_name = Lib_name.to_string (Lib.name (Lib.Local.to_lib local_lib)) in
        Private_lib { unique_name; display_name })
    in
    Memo.return (pkg_items @ private_lib_items)
  ;;

  let get_items ~mode ctx =
    match mode with
    | Odoc_target.Doc_mode.Local_only ->
      let+ packages = Dune_load.packages () in
      of_packages packages
    | Odoc_target.Doc_mode.Full -> get_full_mode_items ctx
  ;;
end

let library_index_content_from_artifacts ~lib_name ~artifacts =
  let b = Buffer.create 256 in
  Printf.bprintf b "@toc_status hidden\n";
  Printf.bprintf b "@order_category libraries\n";
  Printf.bprintf b "{0 Library [%s]}\n" (Lib_name.to_string lib_name);
  (* Extract non-hidden, visible modules from artifacts.
     Odoc_artifact.hidden filters out implementation modules (Foo__Bar, Foo__). *)
  let module_names =
    List.filter_map artifacts ~f:(fun artifact ->
      if Odoc_artifact.hidden artifact
      then None
      else (
        match Odoc_artifact.get_kind artifact with
        | Module ({ visible = true; module_name; _ }, _) -> Some module_name
        | Module ({ visible = false; _ }, _) | Page _ -> None))
    |> List.sort ~compare:Module_name.compare
  in
  if not (List.is_empty module_names)
  then (
    Printf.bprintf b "{!modules:";
    List.iter module_names ~f:(fun m -> Printf.bprintf b " %s" (Module_name.to_string m));
    Printf.bprintf b "}\n");
  Buffer.contents b
;;

(* Generate default package index.mld content from artifacts organized by library.
   Lists entry modules for each library. *)
let default_pkg_index ~pkg ~lib_artifacts =
  let b = Buffer.create 512 in
  Printf.bprintf b "{0 %s index}\n" (Package.Name.to_string pkg);
  let sorted_libs =
    List.sort lib_artifacts ~compare:(fun (lib1, _) (lib2, _) ->
      Lib_name.compare (Lib.name lib1) (Lib.name lib2))
  in
  List.iter sorted_libs ~f:(fun (lib, artifacts) ->
    let lib_name = Lib.name lib in
    let modules =
      List.filter_map artifacts ~f:(fun artifact ->
        if Odoc_artifact.hidden artifact
        then None
        else (
          match Odoc_artifact.get_kind artifact with
          | Module ({ visible = true; module_name; _ }, _) -> Some module_name
          | _ -> None))
      |> List.sort ~compare:Module_name.compare
    in
    if not (List.is_empty modules)
    then (
      Printf.bprintf b "{1 Library %s}\n" (Lib_name.to_string lib_name);
      Buffer.add_string
        b
        (match modules with
         | [ x ] ->
           sprintf
             "The entry point of this library is the module:\n{!/%s/module-%s}.\n"
             (Lib_name.to_string lib_name)
             (Module_name.to_string x)
         | _ ->
           (* TODO: Use qualified paths like {!modules:/lib/Foo /lib/Bar} once odoc
              supports this syntax in the {!modules:} directive. Currently only
              bare module names are supported. *)
           sprintf
             "This library exposes the following toplevel modules:\n{!modules:%s}\n"
             (modules |> List.map ~f:Module_name.to_string |> String.concat ~sep:" "))));
  Buffer.contents b
;;

let create_artifact_module ~target ~local_lib ~module_ ~extra_libs ~extra_packages =
  let mod_ =
    { Odoc_target.visible = Module.visibility module_ = Visibility.Public
    ; module_name = Module_name.Unique.to_name (Module.obj_name module_) ~loc:Loc.none
    }
  in
  let kind = Odoc_artifact.Module (mod_, target) in
  let obj_dir = Lib.Local.obj_dir local_lib in
  let source_file = Obj_dir.Module.cmti_file obj_dir module_ ~cm_kind:(Ocaml Cmi) in
  Odoc_artifact.create
    ~kind
    ~source:(Local_source source_file)
    ~extra_libs
    ~extra_packages
;;

(* Discover modules for a local library and create artifacts.
   Handles both libraries with packages and private libraries (without packages). *)
let discover_local_lib_artifacts sctx ctx ~lib_name ~local_lib
  : Odoc_artifact.t list Memo.t
  =
  let* all_modules = Dir_contents.modules_of_local_lib sctx local_lib in
  let modules = Modules.fold all_modules ~init:[] ~f:(fun m acc -> m :: acc) in
  let info = Lib.Local.info local_lib in
  let pkg = Lib_info.package info in
  let lib_t = Lib.Local.to_lib local_lib in
  (* Get extras from odoc-config for libraries with packages, empty for private libs.
     Also include the library's own package in extra_packages. *)
  let* extra_libs, extra_packages =
    match pkg with
    | None -> Memo.return ([], [])
    | Some pkg ->
      let* pkg_discovery = Package_discovery.create ~context:ctx in
      let+ config_libs, config_packages =
        resolve_pkg_odoc_config ctx ~pkg_discovery ~pkg
      in
      config_libs, pkg :: config_packages
  in
  let target =
    match pkg with
    | None ->
      (* Private library - use lib_unique_name for directory structure *)
      let status = Lib_info.status info in
      let lib_unique_name =
        match status with
        | Lib_info.Status.Private (project, _) ->
          Odoc_scope.Scope_key.to_string lib_name project
        | _ ->
          Lib_name.to_string lib_name (* Fallback, shouldn't happen for private libs *)
      in
      Odoc_target.Private_lib (lib_unique_name, lib_t)
    | Some pkg ->
      (* Library with package - use pkg/lib directory structure *)
      Odoc_target.Lib (pkg, lib_t)
  in
  let artifacts =
    List.map modules ~f:(fun module_ ->
      create_artifact_module ~target ~local_lib ~module_ ~extra_libs ~extra_packages)
  in
  Memo.return artifacts
;;

let toplevel_index_artifact ctx ~mode =
  let output_path = Odoc_paths.toplevel_index_mld ctx mode in
  let page = { Odoc_target.name = "index"; pkg_libs = [] } in
  let kind = Odoc_artifact.Page (page, Odoc_target.Toplevel mode) in
  let* items = Toplevel_index.get_items ~mode ctx in
  let content = Toplevel_index.mld_content items in
  let source = Odoc_artifact.Generated { content; output_path } in
  let package_names =
    List.filter_map items ~f:(fun item ->
      match item with
      | Toplevel_index.Package { name; _ } -> Some (Package.Name.of_string name)
      | Toplevel_index.Private_lib _ -> None)
  in
  let deps = { Odoc_config.packages = package_names; libraries = [] } in
  let* extra_libs, extra_packages_from_deps = resolve_odoc_config_deps ctx ~deps in
  (* Only add private libraries for Full mode - Local_only should only document packages *)
  let* private_lib_pseudo_pkgs =
    match mode with
    | Odoc_target.Doc_mode.Local_only -> Memo.return []
    | Odoc_target.Doc_mode.Full ->
      let+ private_local_libs = get_private_libraries ctx in
      List.map private_local_libs ~f:(fun local_lib ->
        Package.Name.of_string (Odoc_scope.lib_unique_name local_lib))
  in
  let extra_packages = extra_packages_from_deps @ private_lib_pseudo_pkgs in
  Memo.return (Odoc_artifact.create ~kind ~source ~extra_libs ~extra_packages)
;;

(* Create mld artifacts for a package (without index artifacts).
   Index artifacts are created separately after module artifacts are discovered. *)
let discover_pkg_mld_artifacts ~pkg ~pkg_libs ~mld_infos ~extra_libs ~extra_packages =
  let target = Odoc_target.Pkg pkg in
  let mld_artifacts =
    List.map mld_infos ~f:(fun (source, name) ->
      let page = { Odoc_target.name; pkg_libs } in
      let kind = Odoc_artifact.Page (page, target) in
      Odoc_artifact.create ~kind ~source ~extra_libs ~extra_packages)
  in
  let has_index = List.exists mld_infos ~f:(fun (_, name) -> String.equal name "index") in
  mld_artifacts, has_index, mld_infos
;;

let create_pkg_index_artifact
      ctx
      ~pkg
      ~pkg_libs
      ~lib_artifacts
      ~extra_libs
      ~extra_packages
  =
  let target = Odoc_target.Pkg pkg in
  let output_path = Odoc_paths.gen_mld_dir ctx pkg ++ "index.mld" in
  let page = { Odoc_target.name = "index"; pkg_libs } in
  let kind = Odoc_artifact.Page (page, target) in
  let content = default_pkg_index ~pkg ~lib_artifacts in
  let source = Odoc_artifact.Generated { content; output_path } in
  Odoc_artifact.create ~kind ~source ~extra_libs ~extra_packages
;;

let create_lib_index_artifact
      ctx
      ~pkg
      ~pkg_libs
      ~lib_name
      ~lib_artifacts
      ~extra_libs
      ~extra_packages
  =
  let target = Odoc_target.Pkg pkg in
  let lib_index_name = sp "%s/index" (Lib_name.to_string lib_name) in
  let output_path = Odoc_paths.lib_index_mld ctx pkg lib_name in
  let page = { Odoc_target.name = lib_index_name; pkg_libs } in
  let kind = Odoc_artifact.Page (page, target) in
  let content = library_index_content_from_artifacts ~lib_name ~artifacts:lib_artifacts in
  let source = Odoc_artifact.Generated { content; output_path } in
  Odoc_artifact.create ~kind ~source ~extra_libs ~extra_packages
;;

(* Extract page name from installed mld path by finding odoc-pages ancestor.
   E.g., /lib/pkg/odoc-pages/foo/bar.mld -> "foo/bar" *)
let page_name_from_installed_mld_path mld_path =
  let rec find_odoc_pages_ancestor p =
    match Path.parent p with
    | None -> None
    | Some parent ->
      if Path.basename parent = "odoc-pages"
      then Some parent
      else find_odoc_pages_ancestor parent
  in
  match find_odoc_pages_ancestor mld_path with
  | Some odoc_pages_dir ->
    (match Path.drop_prefix mld_path ~prefix:odoc_pages_dir with
     | Some rel_path ->
       let rel_str = Path.Local.to_string rel_path in
       Filename.remove_extension rel_str
     | None -> Path.basename mld_path |> Filename.remove_extension)
  | None -> Path.basename mld_path |> Filename.remove_extension
;;

(* Get archive names for a library (used to filter odoc classify output) *)
let get_archive_names lib_name archives =
  let byte_archives = Mode.Dict.get archives Mode.Byte in
  match byte_archives with
  | [] ->
    if Lib_name.equal lib_name (Lib_name.of_string "stdlib") then [ "stdlib" ] else []
  | archives ->
    List.map archives ~f:(fun p -> Path.basename p |> Filename.remove_extension)
;;

(* Parse odoc classify output to extract module names for specific archives *)
let parse_classify_output ~archive_names classify_content =
  let classify_lines = String.split_lines classify_content in
  List.concat_map classify_lines ~f:(fun line ->
    match
      String.split line ~on:' ' |> List.filter ~f:(fun s -> not (String.is_empty s))
    with
    | [] -> []
    | archive :: mods ->
      if List.mem archive_names archive ~equal:String.equal then mods else [])
;;

let discover_installed_lib_artifacts _sctx ctx ~pkg ~lib_name ~lib
  : Odoc_artifact.t list Memo.t
  =
  let pkg_name_str = Package.Name.to_string pkg in
  let lib_name_str = Lib_name.to_string lib_name in
  let info = Lib.info lib in
  let archive_names = get_archive_names lib_name (Lib_info.archives info) in
  if List.is_empty archive_names
  then (
    Log.info
      [ Pp.textf
          "odoc v3: No archives found for installed library %s/%s, skipping"
          pkg_name_str
          lib_name_str
      ];
    Memo.return [])
  else (
    (* Read and parse classify file to get module names *)
    let classify_path =
      Odoc_paths.root ctx ++ "classify" ++ pkg_name_str ++ lib_name_str ++ "odoc.classify"
    in
    let* classify_content = Build_system.read_file (Path.build classify_path) in
    let all_module_names = parse_classify_output ~archive_names classify_content in
    Log.info
      [ Pp.textf
          "odoc v3: Found %d modules for installed library %s/%s via odoc classify"
          (List.length all_module_names)
          pkg_name_str
          lib_name_str
      ];
    if List.is_empty all_module_names
    then Memo.return []
    else
      let* pkg_discovery = Package_discovery.create ~context:ctx in
      let* extra_libs, extra_packages = resolve_pkg_odoc_config ctx ~pkg_discovery ~pkg in
      let+ all_module_artifacts =
        Memo.parallel_map all_module_names ~f:(fun module_name ->
          match Package_discovery.module_source_file pkg_discovery ~lib ~module_name with
          | Some src_path ->
            let mod_ =
              { Odoc_target.visible = not (String.contains_double_underscore module_name)
              ; module_name = Module_name.of_string module_name
              }
            in
            Memo.return
              (Some
                 (Odoc_artifact.create
                    ~kind:(Module (mod_, Odoc_target.Lib (pkg, lib)))
                    ~source:(Installed_source { src_path })
                    ~extra_libs
                    ~extra_packages))
          | None ->
            Log.info
              [ Pp.textf
                  "odoc v3: Could not find source file for module %s in %s/%s"
                  module_name
                  pkg_name_str
                  lib_name_str
              ];
            Memo.return None)
      in
      List.filter_map all_module_artifacts ~f:Fun.id)
;;

let discover_lib_artifacts sctx ctx ~pkg ~lib_name ~lib : Odoc_artifact.t list Memo.t =
  match Lib.Local.of_lib lib with
  | Some local_lib -> discover_local_lib_artifacts sctx ctx ~lib_name ~local_lib
  | None -> discover_installed_lib_artifacts sctx ctx ~pkg ~lib_name ~lib
;;

let discover_all_lib_artifacts sctx ctx ~pkg ~libs =
  Memo.List.map libs ~f:(fun lib ->
    let lib_name = Lib.name lib in
    let+ artifacts = discover_lib_artifacts sctx ctx ~pkg ~lib_name ~lib in
    lib, artifacts)
;;

let check_mlds_no_dupes ~pkg ~mlds =
  match
    List.map mlds ~f:(fun (mld : Doc_sources.mld) ->
      let in_doc_str = Path.Local.to_string mld.in_doc in
      let name = Filename.remove_extension in_doc_str in
      name, mld.path)
    |> String.Map.of_list
  with
  | Ok _ -> ()
  | Error (_, p1, p2) ->
    User_error.raise
      [ Pp.textf
          "Package %s has two mld's with the same basename %s, %s"
          (Package.Name.to_string pkg)
          (Path.to_string_maybe_quoted (Path.build p1))
          (Path.to_string_maybe_quoted (Path.build p2))
      ]
;;

let get_local_mld_infos sctx ~pkg =
  let+ source_mlds = Packages.mlds sctx pkg in
  check_mlds_no_dupes ~pkg ~mlds:source_mlds;
  List.map source_mlds ~f:(fun (mld : Doc_sources.mld) ->
    let in_doc_str = Path.Local.to_string mld.in_doc in
    let name = Filename.remove_extension in_doc_str in
    let source = Odoc_artifact.Local_source mld.path in
    source, name)
;;

let discover_pkg_artifacts_common
      sctx
      ctx
      ~pkg
      ~libs
      ~mld_infos
      ~extra_libs
      ~extra_packages
      ~generate_lib_indices
  =
  let lib_subdirs = List.map libs ~f:(fun lib -> Lib.name lib |> Lib_name.to_string) in
  let mld_artifacts, has_pkg_index, mld_infos =
    discover_pkg_mld_artifacts ~pkg ~pkg_libs:libs ~mld_infos ~extra_libs ~extra_packages
  in
  let* lib_artifacts = discover_all_lib_artifacts sctx ctx ~pkg ~libs in
  let pkg_index_artifact =
    if has_pkg_index
    then []
    else
      [ create_pkg_index_artifact
          ctx
          ~pkg
          ~pkg_libs:libs
          ~lib_artifacts
          ~extra_libs
          ~extra_packages
      ]
  in
  let lib_index_artifacts =
    if not generate_lib_indices
    then []
    else
      List.filter_map lib_artifacts ~f:(fun (lib, artifacts) ->
        let lib_name = Lib.name lib in
        let lib_index_name = sp "%s/index" (Lib_name.to_string lib_name) in
        let has_source_lib_index =
          List.exists mld_infos ~f:(fun (_, name) -> String.equal name lib_index_name)
        in
        if has_source_lib_index
        then None
        else
          Some
            (create_lib_index_artifact
               ctx
               ~pkg
               ~pkg_libs:libs
               ~lib_name
               ~lib_artifacts:artifacts
               ~extra_libs
               ~extra_packages))
  in
  let all_module_artifacts = List.concat_map lib_artifacts ~f:snd in
  let all_artifacts =
    mld_artifacts @ pkg_index_artifact @ lib_index_artifacts @ all_module_artifacts
  in
  Memo.return (all_artifacts, lib_subdirs)
;;

let create_private_lib_index_artifact ctx ~lib_unique_name ~lib_name ~lib_artifacts =
  let dummy_pkg = Package.Name.of_string lib_unique_name in
  let output_path = Odoc_paths.gen_mld_dir ctx dummy_pkg ++ "index.mld" in
  let page = { Odoc_target.name = "index"; pkg_libs = [] } in
  let kind = Odoc_artifact.Page (page, Odoc_target.Pkg dummy_pkg) in
  let content = library_index_content_from_artifacts ~lib_name ~artifacts:lib_artifacts in
  let source = Odoc_artifact.Generated { content; output_path } in
  Odoc_artifact.create ~kind ~source ~extra_libs:[] ~extra_packages:[]
;;

(* Discover artifacts for a private library.
   Takes the fields from Scope_id.Private_lib directly to make invalid calls impossible. *)
let discover_private_lib_artifacts sctx ctx ~lib_unique_name ~lib_name ~project
  : (Odoc_artifact.t list * string list) Memo.t
  =
  let* lib_db =
    let+ scope = Scope.DB.find_by_project (Context.name ctx) project in
    Scope.libs scope
  in
  let* lib_opt =
    let+ lib = Lib.DB.find lib_db lib_name in
    Option.bind ~f:Lib.Local.of_lib lib
  in
  match lib_opt with
  | None -> Memo.return ([], [])
  | Some local_lib ->
    let* module_artifacts = discover_local_lib_artifacts sctx ctx ~lib_name ~local_lib in
    let index_artifact =
      create_private_lib_index_artifact
        ctx
        ~lib_unique_name
        ~lib_name
        ~lib_artifacts:module_artifacts
    in
    let artifacts = index_artifact :: module_artifacts in
    (* Private libraries don't have subdirectories in the same sense as packages *)
    Memo.return (artifacts, [])
;;

let discover_local_pkg_artifacts sctx ctx ~pkg
  : (Odoc_artifact.t list * string list) Memo.t
  =
  let* all_libs = libs_of_pkg ctx ~pkg in
  let libs =
    List.filter_map all_libs ~f:(fun lib ->
      Option.map (Lib.Local.of_lib lib) ~f:Lib.Local.to_lib)
  in
  let* pkg_discovery = Package_discovery.create ~context:ctx in
  let* extra_libs, extra_packages = resolve_pkg_odoc_config ctx ~pkg_discovery ~pkg in
  let* mld_infos = get_local_mld_infos sctx ~pkg in
  discover_pkg_artifacts_common
    sctx
    ctx
    ~pkg
    ~libs
    ~mld_infos
    ~extra_libs
    ~extra_packages
    ~generate_lib_indices:true
;;

let discover_installed_pkg_artifacts sctx ctx ~pkg
  : (Odoc_artifact.t list * string list) Memo.t
  =
  let* pkg_discovery = Package_discovery.create ~context:ctx in
  let libs = Package_discovery.libraries_of_package pkg_discovery pkg in
  Log.info
    [ Pp.textf
        "discover_installed_pkg_artifacts(%s): got %d libs"
        (Package.Name.to_string pkg)
        (List.length libs)
    ];
  let* extra_libs, extra_packages = resolve_pkg_odoc_config ctx ~pkg_discovery ~pkg in
  let mld_files = Package_discovery.mlds_of_package pkg_discovery pkg in
  let mld_infos =
    List.map mld_files ~f:(fun mld_path ->
      let name = page_name_from_installed_mld_path mld_path in
      let source = Odoc_artifact.Installed_source { src_path = mld_path } in
      source, name)
  in
  discover_pkg_artifacts_common
    sctx
    ctx
    ~pkg
    ~libs
    ~mld_infos
    ~extra_libs
    ~extra_packages
    ~generate_lib_indices:false
;;

(* Discover ALL artifacts for a package identifier (either a package name or private lib_unique_name).
   For packages: returns artifacts for all libraries in the package + package-level mld files
   For private libraries: returns artifacts for all modules in the library (no mld files)

   This is the unified entry point that all handlers (odoc, odocls, html) should use. *)
let discover_package_artifacts sctx ctx ~pkg_or_lib_unique_name
  : (Odoc_artifact.t list * string list) Memo.t
  =
  let* scope_id = Odoc_scope.Scope_id.of_string pkg_or_lib_unique_name in
  match scope_id with
  | Odoc_scope.Scope_id.Private_lib { unique_name; lib_name; project } ->
    discover_private_lib_artifacts
      sctx
      ctx
      ~lib_unique_name:unique_name
      ~lib_name
      ~project
  | Odoc_scope.Scope_id.Package pkg ->
    let* is_project_pkg =
      let* packages = Dune_load.packages () in
      Memo.return (Package.Name.Map.mem packages pkg)
    in
    Log.info
      [ Pp.textf
          "discover_package_artifacts(%s): is_project_pkg=%b"
          pkg_or_lib_unique_name
          is_project_pkg
      ];
    if is_project_pkg
    then discover_local_pkg_artifacts sctx ctx ~pkg
    else discover_installed_pkg_artifacts sctx ctx ~pkg
;;

(* Collect all visible odocl files from packages (and private libraries for Full mode),
   including toplevel index.
   For Full mode, includes all transitive dependencies (installed packages).
   For Local_only mode, only includes workspace packages. *)
let collect_all_visible_odocls sctx ~mode () =
  let ctx = Super_context.context sctx in
  let* workspace_pkgs = get_workspace_packages () in
  (* Only get private libraries for Full mode - Local_only should only document packages *)
  let* private_local_libs =
    match mode with
    | Odoc_target.Doc_mode.Local_only -> Memo.return []
    | Odoc_target.Doc_mode.Full -> get_private_libraries ctx
  in
  let private_libs = List.map private_local_libs ~f:Lib.Local.to_lib in
  let* packages_to_collect =
    match mode with
    | Odoc_target.Doc_mode.Full ->
      expand_packages_with_odoc_config ctx ~packages:workspace_pkgs ~private_libs
    | Odoc_target.Doc_mode.Local_only ->
      Memo.return (Package.Name.Set.of_list workspace_pkgs)
  in
  let* pkg_odocl_files =
    Memo.List.concat_map (Package.Name.Set.to_list packages_to_collect) ~f:(fun pkg ->
      let pkg_name = Package.Name.to_string pkg in
      let* all_artifacts, _lib_subdirs =
        discover_package_artifacts sctx ctx ~pkg_or_lib_unique_name:pkg_name
      in
      Memo.return
        (List.filter_map all_artifacts ~f:(fun artifact ->
           if Odoc_artifact.hidden artifact
           then None
           else Some (Odoc_artifact.odocl_file ctx artifact))))
  in
  let* private_lib_odocl_files =
    Memo.List.concat_map private_local_libs ~f:(fun local_lib ->
      let lib_unique_name = Odoc_scope.lib_unique_name local_lib in
      let* all_artifacts, _lib_subdirs =
        discover_package_artifacts sctx ctx ~pkg_or_lib_unique_name:lib_unique_name
      in
      Memo.return
        (List.filter_map all_artifacts ~f:(fun artifact ->
           if Odoc_artifact.hidden artifact
           then None
           else Some (Odoc_artifact.odocl_file ctx artifact))))
  in
  let* toplevel_artifact = toplevel_index_artifact ctx ~mode in
  let toplevel_odocl = Odoc_artifact.odocl_file ctx toplevel_artifact in
  let all_odocl_files = (toplevel_odocl :: pkg_odocl_files) @ private_lib_odocl_files in
  Memo.return (workspace_pkgs, all_odocl_files)
;;
