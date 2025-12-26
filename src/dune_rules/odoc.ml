open Import
open Memo.O
module Gen_rules = Build_config.Gen_rules

let ( ++ ) = Path.Build.relative

module Target = Odoc_target
module Paths = Odoc_paths
module Doc_mode = Paths.Doc_mode
module Artifact = Odoc_artifact
module Scope_id = Odoc_scope.Scope_id

(* Type for odoc compilation/linking stages (used by lib_dir_path) *)
type odoc_output =
  | Odoc
  | Odocls

(* ============================================================================
   BUILD UTILITIES - Rules, formats, and dependencies
   ============================================================================ *)

let add_rule sctx =
  let dir = Super_context.context sctx |> Context.build_dir in
  Super_context.add_rule sctx ~dir
;;

module Output_format = struct
  type t =
    | Html
    | Json

  let all = [ Html; Json ]

  let other = function
    | Html -> Json
    | Json -> Html
  ;;

  let args = function
    | Html -> Command.Args.empty
    | Json -> A "--as-json"
  ;;

  let target ctx mode t odoc_file =
    match t with
    | Html -> Artifact.html_file ctx mode odoc_file
    | Json -> Artifact.json_file ctx mode odoc_file
  ;;

  let dir_target ctx mode t artifact =
    match t with
    | Html -> Artifact.html_dir_target ctx mode artifact
    | Json -> Artifact.json_dir_target ctx mode artifact
  ;;

  let to_paths_format = function
    | Html -> Paths.Html
    | Json -> Paths.Json
  ;;

  let alias t ~mode ~dir =
    match t, mode with
    | Html, Doc_mode.Local_only -> Alias.make Alias0.doc ~dir
    | Html, Doc_mode.Full -> Alias.make Alias0.doc_full ~dir
    | Json, Doc_mode.Local_only -> Alias.make Alias0.doc_json ~dir
    | Json, Doc_mode.Full -> Alias.make Alias0.doc_json_full ~dir
  ;;
end

module Dep : sig
  (** Create a .odoc-all alias at any directory *)
  val odoc_all_alias : dir:Path.Build.t -> Alias.t

  (** Create a format alias (.doc or .doc-json) for a target *)
  val format_alias : Output_format.t -> Doc_mode.t -> Context.t -> 'a Target.t -> Alias.t

  (** Add file dependencies to an alias *)
  val add_file_deps : Alias.t -> Path.t list -> unit Memo.t

  (** Make an alias depend on .odoc-all aliases in the given directories *)
  val add_odoc_all_deps : Alias.t -> dirs:Path.Build.t list -> unit Memo.t

  (** High-level: Get dependencies for libraries during compilation/linking *)
  val deps
    :  Context.t
    -> Package.Name.t list
    -> Lib.t list Resolve.t
    -> unit Action_builder.t

  (** High-level: Set up .odoc-all dependencies for a target *)
  val setup_deps : Context.t -> 'a Target.t -> Path.Set.t -> unit Memo.t
end = struct
  let odoc_all_alias ~dir = Alias.make (Alias.Name.of_string ".odoc-all") ~dir

  let odoc_all_alias_for_target : type a. Context.t -> a Target.t -> Alias.t =
    fun ctx target -> odoc_all_alias ~dir:(Paths.odocs ctx target)
  ;;

  let format_alias
    : type a. Output_format.t -> Doc_mode.t -> Context.t -> a Target.t -> Alias.t
    =
    fun f mode ctx m ->
    let dir =
      match f with
      | Html -> Paths.html ctx mode m
      | Json -> Paths.json ctx mode m
    in
    Output_format.alias f ~mode ~dir
  ;;

  let add_file_deps alias files =
    Rules.Produce.Alias.add_deps alias (Action_builder.paths files)
  ;;

  let add_odoc_all_deps alias ~dirs =
    let dep_set =
      List.map dirs ~f:(fun dir -> Dune_engine.Dep.alias (odoc_all_alias ~dir))
      |> Dune_engine.Dep.Set.of_list
    in
    Rules.Produce.Alias.add_deps alias (Action_builder.deps dep_set)
  ;;

  let deps ctx pkgs requires =
    let open Action_builder.O in
    let* libs = Resolve.read requires in
    let* pkg_discovery = Action_builder.of_memo (Package_discovery.create ~context:ctx) in
    Action_builder.deps
      (let init =
         List.fold_left pkgs ~init:Dep.Set.empty ~f:(fun acc p ->
           Dep.Set.add acc (Dep.alias (odoc_all_alias ~dir:(Paths.odocs ctx (Pkg p)))))
       in
       List.fold_left libs ~init ~f:(fun acc (lib : Lib.t) ->
         match Lib.Local.of_lib lib with
         | None ->
           let lib_pkg_opt = Package_discovery.package_of_library pkg_discovery lib in
           (match lib_pkg_opt with
            | Some lib_pkg ->
              let dir =
                Paths.root ctx
                ++ "_odoc"
                ++ Package.Name.to_string lib_pkg
                ++ Lib_name.to_string (Lib.name lib)
              in
              Dep.Set.add acc (Dep.alias (odoc_all_alias ~dir))
            | None -> acc)
         | Some local_lib ->
           let lib_t = Lib.Local.to_lib local_lib in
           let info = Lib.info lib_t in
           (* Skip implementations of virtual libraries - they don't have docs *)
           (match Lib_info.implements info with
            | Some _ -> acc
            | None ->
              let target =
                match Lib_info.package info with
                | Some pkg -> Target.Lib (pkg, lib_t)
                | None ->
                  let lib_unique_name = Odoc_scope.lib_unique_name local_lib in
                  Target.Private_lib (lib_unique_name, lib_t)
              in
              let dir = Paths.odocs ctx target in
              Dep.Set.add acc (Dep.alias (odoc_all_alias ~dir)))))
  ;;

  let setup_deps : type a. Context.t -> a Target.t -> Path.Set.t -> unit Memo.t =
    fun ctx m files ->
    add_file_deps (odoc_all_alias_for_target ctx m) (Path.Set.to_list files)
  ;;
end

let get_workspace_packages = Odoc_discovery.get_workspace_packages

(* ============================================================================
   REMAP - URL remapping for external packages
   ============================================================================ *)

(* Generate remap mappings for external (non-local) packages *)
let generate_remap_mappings ctx pkg_discovery ~packages =
  (* Filter to non-local (installed) packages that need remapping *)
  let* packages_to_remap =
    Memo.List.filter (Package.Name.Set.to_list packages) ~f:(fun pkg ->
      let+ is_local = Odoc_discovery.is_local_package pkg in
      not is_local)
  in
  (* Generate mappings for installed packages *)
  let* mappings =
    Memo.List.map packages_to_remap ~f:(fun pkg_name ->
      let* version_opt = Package_discovery.version_of_package pkg_discovery pkg_name in
      let version = Option.value version_opt ~default:"latest" in
      let pkg_path = Package.Name.to_string pkg_name in
      let pkg_url = Printf.sprintf "https://ocaml.org/p/%s/%s/doc/" pkg_path version in
      (* Get libraries in this package for lib-level mappings *)
      let* libs = Odoc_discovery.libs_of_pkg ctx ~pkg:pkg_name in
      let lib_mappings =
        List.map libs ~f:(fun lib ->
          let lib_name = Lib_name.to_string (Lib.name lib) in
          let lib_path = pkg_path ^ "/" ^ lib_name in
          let lib_url = pkg_url ^ lib_name in
          lib_path, lib_url)
      in
      Memo.return ((pkg_path ^ "/", pkg_url) :: lib_mappings))
  in
  Memo.return (List.concat mappings)
;;

(* Write remap file with given mappings *)
let write_remap_file sctx ~remap_file ~mappings =
  let contents =
    String.concat
      ~sep:"\n"
      (List.map mappings ~f:(fun (local, remote) -> Printf.sprintf "%s:%s" local remote))
  in
  add_rule sctx (Action_builder.write_file remap_file contents)
;;

(* ============================================================================
   CONFIGURATION - Flags and settings
   ============================================================================ *)

module Flags = struct
  type warnings = Dune_env.Odoc.warnings =
    | Fatal
    | Nonfatal

  type sidebar = Dune_env.Odoc.sidebar =
    | Global
    | Per_package

  type support = Dune_env.Odoc.support =
    | Root
    | Per_package

  type t =
    { warnings : warnings
    ; sidebar : sidebar
    ; support : support
    }

  let default = { warnings = Nonfatal; sidebar = Global; support = Root }

  let get_memo ~dir =
    Env_stanza_db.value ~default ~dir ~f:(fun config ->
      let warnings = Option.value config.odoc.warnings ~default:default.warnings in
      let sidebar = Option.value config.odoc.sidebar ~default:default.sidebar in
      let support = Option.value config.odoc.support ~default:default.support in
      Memo.return (Some { warnings; sidebar; support }))
  ;;

  let get ~dir = get_memo ~dir |> Action_builder.of_memo
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

(* ============================================================================
   ODOC EXECUTION - Running odoc commands
   ============================================================================ *)

let odoc_dev_tool_exe_path_building_if_necessary () =
  let open Action_builder.O in
  let path = Path.build (Pkg_dev_tool.exe_path Odoc) in
  let+ () = Action_builder.path path in
  Ok path
;;

let odoc_program sctx dir =
  let odoc_dev_tool_lock_dir_exists =
    match Config.get Compile_time.lock_dev_tools with
    | `Enabled -> true
    | `Disabled -> false
  in
  match odoc_dev_tool_lock_dir_exists with
  | true -> odoc_dev_tool_exe_path_building_if_necessary ()
  | false ->
    Super_context.resolve_program
      sctx
      ~dir
      ~where:Original_path
      "odoc"
      ~loc:None
      ~hint:"opam install odoc"
;;

let run_odoc sctx ?dir command ~quiet ~flags_for args =
  let ctx = Super_context.context sctx in
  let build_dir = Context.build_dir ctx in
  let program = odoc_program sctx build_dir in
  let dir = Path.build (Option.value dir ~default:(Paths.root ctx)) in
  let base_flags =
    let open Action_builder.O in
    let* () = Action_builder.return () in
    match flags_for with
    | None -> Action_builder.return Command.Args.empty
    | Some path -> odoc_base_flags quiet path
  in
  (* Depend on ODOC_SYNTAX env var and the odoc binary itself.
     The binary dependency ensures rules rebuild when odoc is updated. *)
  let deps =
    let open Action_builder.O in
    let* () = Action_builder.env_var "ODOC_SYNTAX" in
    let* prog_result = program in
    match prog_result with
    | Ok path -> Action_builder.path path
    | Error _ -> Action_builder.return ()
  in
  let open Action_builder.With_targets.O in
  let run =
    Action_builder.with_no_targets deps
    >>> Command.run_dyn_prog ~dir program [ A command; Dyn base_flags; S args ]
  in
  (* For external artifacts (quiet=true), suppress both stdout and stderr *)
  if quiet
  then
    Action_builder.With_targets.map run ~f:(fun action ->
      Action.Full.map action ~f:Action.ignore_outputs)
  else run
;;

(* Common helper to get library paths with optional stdlib *)
let get_lib_paths ctx ~stdlib_opt requires pkg_discovery =
  let open Resolve.O in
  let+ libs = requires in
  (* Add stdlib to the list of libraries if provided and not already present *)
  let libs =
    match stdlib_opt with
    | Some stdlib ->
      if List.exists libs ~f:(fun lib -> Lib_name.equal (Lib.name lib) (Lib.name stdlib))
      then libs
      else stdlib :: libs
    | None -> libs
  in
  List.filter_map libs ~f:(fun lib ->
    match Lib.Local.of_lib lib with
    | None ->
      (* Installed library *)
      let lib_pkg_opt = Package_discovery.package_of_library pkg_discovery lib in
      Option.map lib_pkg_opt ~f:(fun lib_pkg ->
        lib, Paths.odocs ctx (Target.Lib (lib_pkg, lib)))
    | Some local_lib ->
      (* Local library *)
      let lib_t = Lib.Local.to_lib local_lib in
      let lib_info = Lib.info lib_t in
      let target =
        match Lib_info.package lib_info with
        | Some pkg -> Target.Lib (pkg, lib_t)
        | None ->
          let lib_unique_name = Odoc_scope.lib_unique_name local_lib in
          Target.Private_lib (lib_unique_name, lib_t)
      in
      Some (lib, Paths.odocs ctx target))
;;

let stdlib_lib ctx =
  let* public_libs = Scope.DB.public_libs ctx in
  Lib.DB.find public_libs (Lib_name.of_string "stdlib")
;;

let odoc_include_flags ctx pkg requires pkg_discovery =
  let open Memo.O in
  let* stdlib_opt = stdlib_lib (Context.name ctx) in
  let args =
    Resolve.args
      (let open Resolve.O in
       let+ lib_paths = get_lib_paths ctx ~stdlib_opt requires pkg_discovery in
       let paths =
         List.fold_left lib_paths ~init:Path.Set.empty ~f:(fun paths (_lib, path) ->
           Path.Set.add paths (Path.build path))
       in
       let paths =
         match pkg with
         | Some p -> Path.Set.add paths (Path.build (Paths.odocs ctx (Pkg p)))
         | None -> paths
       in
       Command.Args.S
         (List.concat_map (Path.Set.to_list paths) ~f:(fun dir ->
            [ Command.Args.A "-I"; Path dir ])))
  in
  Memo.return args
;;

(* Generate -L library:path flags for odoc link
   These tell odoc where to find .odocl files for library dependencies *)
let odoc_lib_flags ctx ~stdlib_opt requires pkg_discovery =
  Resolve.args
    (let open Resolve.O in
     let+ lib_paths = get_lib_paths ctx ~stdlib_opt requires pkg_discovery in
     (* Deduplicate by library name and make paths relative *)
     let doc_root = Paths.root ctx in
     let lib_paths_map =
       List.fold_left lib_paths ~init:Lib_name.Map.empty ~f:(fun acc (lib, odoc_dir) ->
         let lib_name = Lib.name lib in
         if Lib_name.Map.mem acc lib_name
         then acc
         else (
           let lib_name_str = Lib_name.to_string lib_name in
           (* Compute path relative to doc_root using proper path functions *)
           let odoc_path_rel =
             Path.reach (Path.build odoc_dir) ~from:(Path.build doc_root)
           in
           let lib_path_arg = lib_name_str ^ ":" ^ odoc_path_rel in
           Lib_name.Map.set acc lib_name lib_path_arg))
     in
     (* Convert map to args *)
     let lib_args =
       Lib_name.Map.values lib_paths_map
       |> List.concat_map ~f:(fun lib_path_arg -> [ Command.Args.A "-L"; A lib_path_arg ])
     in
     Command.Args.S lib_args)
;;

(* Get package dependencies from odoc-config.sexp for a set of packages *)
let get_config_package_deps pkg_discovery pkgs =
  List.concat_map pkgs ~f:(fun pkg ->
    let config = Package_discovery.config_of_package pkg_discovery pkg in
    config.Odoc_config.deps.packages)
;;

(* Generate -P package:path flags for odoc link.
   These tell odoc where to find .odoc files for package dependencies. *)
let odoc_pkg_flags ctx pkg_discovery ~current_pkg_opt ~artifact_config =
  let doc_root = Paths.root ctx in
  (* Collect direct packages (config deps + current package) and their transitive config deps *)
  let direct_pkgs =
    artifact_config.Odoc_config.deps.packages @ Option.to_list current_pkg_opt
  in
  let all_pkgs = direct_pkgs @ get_config_package_deps pkg_discovery direct_pkgs in
  (* Build unique package map with their odoc paths *)
  let pkg_paths =
    List.fold_left all_pkgs ~init:Package.Name.Map.empty ~f:(fun acc pkg ->
      let odoc_dir = Paths.odocs ctx (Pkg pkg) in
      let path = Path.reach (Path.build odoc_dir) ~from:(Path.build doc_root) in
      Package.Name.Map.set acc pkg path)
  in
  (* Generate -P pkg:path flags *)
  let flags =
    Package.Name.Map.to_list_map pkg_paths ~f:(fun pkg path ->
      [ Command.Args.A "-P"; A (Package.Name.to_string pkg ^ ":" ^ path) ])
    |> List.concat
  in
  Command.Args.S flags
;;

(* ============================================================================
   COMPILATION - Compiling, linking, and HTML generation
   ============================================================================ *)

(* Compute library dependencies for an artifact.

   Returns a pair (closure, external_requires):
   - closure: Full transitive closure of library dependencies including self
   - external_requires: Transitive dependencies excluding self and same-package libraries

   Handles the special case of stdlib: when compiling stdlib itself, we don't add
   stdlib to its own dependency list to avoid cycles. *)
let compute_artifact_library_deps ctx ~artifact ~package_lib_names =
  (* Get stdlib for dependency resolution *)
  let* stdlib_opt = stdlib_lib (Context.name ctx) in
  (* Check if this artifact is part of stdlib *)
  let is_stdlib_artifact =
    match stdlib_opt with
    | Some stdlib -> Lib_name.equal (Lib.name stdlib) (Artifact.lib_name artifact)
    | None -> false
  in
  (* Get TRANSITIVE closure of dependencies (not just direct requires) *)
  let* closure =
    match Artifact.lib artifact with
    | Some lib ->
      (* Include stdlib in the closure unless we're compiling stdlib itself *)
      let libs_to_close =
        if is_stdlib_artifact then [ lib ] else lib :: Option.to_list stdlib_opt
      in
      Lib.closure libs_to_close ~linking:false
    | None -> Memo.return (Resolve.return [])
    (* Package-level and toplevel artifacts have no library dependencies *)
  in
  (* For library dependency aliases, filter out:
     1. The library itself (to avoid self-dependency)
     2. Other libraries in the same package (to avoid circular dependencies)
     Module-level dependencies from odoc compile-deps will handle the actual file dependencies.
     This prevents circular dependencies when libraries in the same package have circular module deps
     (e.g., OCaml's compiler-libs where compiler-libs.common's Meta depends on compiler-libs.bytecomp's Instruct). *)
  let external_requires =
    match Artifact.lib artifact with
    | Some lib ->
      Resolve.map closure ~f:(fun all_libs ->
        List.filter all_libs ~f:(fun dep_lib ->
          let dep_lib_name = Lib.name dep_lib in
          (not (Lib_name.equal dep_lib_name (Lib.name lib)))
          && not (Lib_name.Set.mem package_lib_names dep_lib_name)))
    | None -> closure
  in
  Memo.return (closure, external_requires)
;;

(* Compute intra-library module dependencies using odoc compile-deps.

   For Module artifacts, generates a .deps file and parses it to find which other
   modules in the same library this module depends on. Returns an Action_builder
   that creates dependencies on the .odoc files of those modules.

   For Page artifacts, returns an empty Action_builder since pages don't have module deps. *)
let compute_intra_library_module_deps sctx ~ctx ~artifact ~lib_artifacts_by_module =
  let source_file = Artifact.source_file artifact in
  match Artifact.get_kind artifact with
  | Page _ ->
    (* Pages don't have module dependencies *)
    Memo.return (Action_builder.return ())
  | Module ({ module_name; _ }, _) ->
    (* Generate deps file using odoc compile-deps *)
    let module_name_str = Module_name.to_string module_name in
    let output_dir = Artifact.odoc_dir ctx artifact in
    let deps_file = Path.Build.relative output_dir (module_name_str ^ ".deps") in
    let program = odoc_program sctx (Context.build_dir ctx) in
    (* Generate compile-deps rule *)
    let* () =
      let run_compile_deps =
        Command.run_dyn_prog
          program
          ~dir:(Path.build (Context.build_dir ctx))
          ~stdout_to:deps_file
          [ A "compile-deps"; Dep source_file ]
      in
      add_rule sctx run_compile_deps
    in
    (* Parse deps file and create dependencies on .odoc files.
       NOTE: odoc compile-deps returns ALL module dependencies, including from other libraries.
       However, inter-library dependencies are already handled by lib_deps below via Dep.deps,
       so here we ONLY handle intra-library dependencies (modules in the same library).

       We check if each dependency module is in Artifact.lib_modules artifact.
       If it's not found, it's an inter-library dependency and we skip it. *)
    Memo.return
      (let open Action_builder.O in
       let* lines = Action_builder.lines_of (Path.build deps_file) in
       let dep_modules =
         List.filter_map lines ~f:(fun line ->
           match String.split ~on:' ' line with
           | [ m; _hash ] -> Some (Module_name.of_string m)
           | _ -> None)
       in
       (* Find .odoc files for dependencies in the same library using the prebuilt map *)
       let current_module_name =
         match Artifact.get_kind artifact with
         | Module ({ module_name; _ }, _) -> Some module_name
         | Page _ -> None
       in
       let dep_odoc_files =
         List.filter_map dep_modules ~f:(fun dep_module ->
           (* Skip self-dependencies *)
           match current_module_name with
           | Some current when Module_name.equal current dep_module -> None
           | _ -> Module_name.Map.find lib_artifacts_by_module dep_module)
       in
       Dune_engine.Dep.Set.of_files dep_odoc_files |> Action_builder.deps)
;;

(* Unified compilation function that computes all dependencies from the artifact.
   This replaces the separate compile_module, compile_mld, compile_installed_module_artifact functions.
   All information needed for compilation is now in the artifact itself.

   For Module artifacts, we use `odoc compile-deps` to determine intra-library module dependencies.
   For Page artifacts (mld files), we compile directly without module dependencies. *)
let compile_artifact sctx ~artifact ~lib_artifacts_by_module ~package_lib_names =
  let ctx = Super_context.context sctx in
  let source_file = Artifact.source_file artifact in
  (* For Module artifacts, run compile-deps to find intra-library module dependencies *)
  let* module_deps =
    compute_intra_library_module_deps sctx ~ctx ~artifact ~lib_artifacts_by_module
  in
  (* Compute library dependencies from the artifact's target *)
  let* closure, external_requires =
    compute_artifact_library_deps ctx ~artifact ~package_lib_names
  in
  let* pkg_discovery = Package_discovery.create ~context:ctx in
  let* include_flags = odoc_include_flags ctx None closure pkg_discovery in
  (* For installed packages or vendored libraries, suppress output (both stdout and stderr) *)
  let* should_suppress = Artifact.should_suppress_output artifact in
  (* Create dependencies on all required libraries' .odoc files (via .odoc-all aliases)
     IMPORTANT: Pass empty list for pkg during compilation to avoid creating a dependency cycle
     on our own package's .odoc-all alias. The package alias is only needed during linking.
     Use external_requires which excludes self and same-package libraries. *)
  let lib_deps = Dep.deps ctx [] external_requires in
  let run_odoc =
    let open Action_builder.With_targets.O in
    (* Depend on: 1) intra-library module deps, 2) inter-library deps *)
    Action_builder.with_no_targets module_deps
    >>> Action_builder.with_no_targets lib_deps
    >>> Action_builder.With_targets.add
          ~file_targets:[ Artifact.odoc_file ctx artifact ]
          (run_odoc
             sctx
             "compile"
             ~quiet:should_suppress
             ~flags_for:(Some (Artifact.odoc_file ctx artifact))
             [ (* Include paths for all dependency libraries including stdlib and current library *)
               include_flags
             ; (* Use --output-dir + --parent-id for artifacts with parents,
                  or -o for toplevel without parent *)
               (let parent = Artifact.parent_id artifact in
                if String.is_empty parent
                then
                  Command.Args.S
                    [ Command.Args.A "-o"
                    ; Command.Args.Target (Artifact.odoc_file ctx artifact)
                    ]
                else
                  Command.Args.S
                    [ Command.Args.A "--output-dir"
                    ; Command.Args.A "_odoc"
                    ; Command.Args.A "--parent-id"
                    ; Command.Args.A parent
                    ])
             ; Command.Args.A "--enable-missing-root-warning"
             ; (match Artifact.get_kind artifact with
                | Module (_, Lib (pkg, _)) ->
                  Command.Args.As [ "--warnings-tag"; Package.Name.to_string pkg ]
                | Module (_, Private_lib _) ->
                  Command.Args.As [ "--warnings-tag"; "__private_lib__" ]
                | Page (_, Pkg pkg) ->
                  Command.Args.As [ "--warnings-tag"; Package.Name.to_string pkg ]
                | Page (_, Toplevel _) -> Command.Args.S [])
             ; Command.Args.Dep source_file
             ])
  in
  add_rule sctx run_odoc
;;

let link_odoc_rules sctx (odoc_file : Artifact.t) ~pkg ~requires =
  let ctx = Super_context.context sctx in
  (* Collect all packages we need dependencies for: current package + config packages *)
  let all_pkgs = Option.to_list pkg @ Artifact.extra_packages odoc_file in
  let deps = Dep.deps ctx all_pkgs requires in
  let* stdlib_opt = stdlib_lib (Context.name ctx) in
  let* pkg_discovery = Package_discovery.create ~context:ctx in
  (* Get all packages in the workspace to pass as --warnings-tags *)
  let* workspace_pkgs = get_workspace_packages () in
  let all_pkg_names = List.map workspace_pkgs ~f:Package.Name.to_string in
  (* Build --warnings-tags arguments for all packages, plus __private_lib__ for private libraries *)
  let warnings_tags_args =
    Command.Args.S
      (List.concat_map ("__private_lib__" :: all_pkg_names) ~f:(fun pkg_name ->
         [ Command.Args.A "--warnings-tags"; Command.Args.A pkg_name ]))
  in
  (* Suppress output for installed packages and vendored libraries *)
  let* quiet = Artifact.should_suppress_output odoc_file in
  (* Note: odoc_lib_flags handles -L flags for all libraries in requires,
     including the library itself (which compute_link_requires adds to requires) *)
  let artifact_config =
    { Odoc_config.deps = { packages = Artifact.extra_packages odoc_file; libraries = [] }
    }
  in
  let run_odoc =
    run_odoc
      sctx
      "link"
      ~quiet
      ~flags_for:(Some (Artifact.odoc_file ctx odoc_file))
      [ odoc_lib_flags ctx ~stdlib_opt requires pkg_discovery
      ; odoc_pkg_flags
          ctx
          pkg_discovery
          ~current_pkg_opt:(Artifact.pkg odoc_file)
          ~artifact_config
      ; (* Add --current-package flag when we have a package *)
        (match Artifact.pkg odoc_file with
         | Some pkg_name ->
           Command.Args.As [ "--current-package"; Package.Name.to_string pkg_name ]
         | None -> Command.Args.S [])
      ; A "--enable-missing-root-warning"
      ; warnings_tags_args
      ; A "-o"
      ; Target (Artifact.odocl_file ctx odoc_file)
      ; Dep (Path.build (Artifact.odoc_file ctx odoc_file))
      ]
  in
  add_rule
    sctx
    (let open Action_builder.With_targets.O in
     Action_builder.with_no_targets deps >>> run_odoc)
;;

(* Unified HTML/JSON generation function for artifacts.
   Takes an artifact, search_db, and optional sidebar file, generates output for it.
   This follows the same pattern as compile_artifact and link_artifact.
   Mode parameter determines output directory and whether to use remap file.
   pkg_name is used for per-package support files - when None (toplevel), always uses root. *)
let generate_html_artifact
      sctx
      ~artifact
      ?search_db
      ~sidebar_file
      ?(remap_file : Path.Build.t option = None)
      ?(mode = Doc_mode.Local_only)
      ~output_format
      ?pkg_name
      ()
  =
  let ctx = Super_context.context sctx in
  let html_root = Paths.html_root ctx mode in
  let json_root = Paths.json_root ctx mode in
  let* flags = Flags.get_memo ~dir:(Context.build_dir ctx) in
  (* Determine support path: per-package only when configured AND we have a package *)
  let odoc_support_path, odoc_support_uri, sherlodoc_js_dir =
    match flags.support, pkg_name with
    | Flags.Per_package, Some pkg ->
      let support_path = Paths.odoc_support_for_pkg ctx mode pkg in
      let pkg_html_dir = html_root ++ pkg in
      let uri = Path.reach (Path.build support_path) ~from:(Path.build html_root) in
      support_path, uri, pkg_html_dir
    | Flags.Root, _ | Flags.Per_package, None ->
      let support_path = Paths.odoc_support ctx mode in
      let uri = Path.reach (Path.build support_path) ~from:(Path.build html_root) in
      support_path, uri, html_root
  in
  let doc_root = Paths.root ctx in
  (* Compute relative paths from doc_root (_doc) for working directory paths *)
  let html_root_rel = Path.reach (Path.build html_root) ~from:(Path.build doc_root) in
  let json_root_rel = Path.reach (Path.build json_root) ~from:(Path.build doc_root) in
  let search_args =
    match search_db with
    | Some search_db ->
      (* Use per-package HTML dir for sherlodoc.js when configured *)
      Sherlodoc.odoc_args
        sctx
        ~search_db
        ~dir_sherlodoc_dot_js:sherlodoc_js_dir
        ~html_root
    | None -> Command.Args.empty
  in
  let output_file = Output_format.target ctx mode output_format artifact in
  (* Use different output directories for HTML vs JSON *)
  let output_root_rel =
    match output_format with
    | Html -> html_root_rel
    | Json -> json_root_rel
  in
  (* Suppress output for installed packages *)
  let* quiet = Artifact.should_suppress_output artifact in
  let run_odoc =
    run_odoc
      sctx
      "html-generate"
      ~quiet
      ~flags_for:None
      [ search_args
      ; A "-o"
      ; A output_root_rel
      ; A "--support-uri"
      ; A odoc_support_uri
      ; A "--theme-uri"
      ; A odoc_support_uri
      ; (match remap_file with
         | None -> S []
         | Some rf -> S [ A "--remap-file"; Dep (Path.build rf) ])
      ; (match sidebar_file with
         | Some sf -> S [ A "--sidebar"; Dep (Path.build sf) ]
         | None -> S [])
      ; Dep (Path.build (Artifact.odocl_file ctx artifact))
      ; Output_format.args output_format
      ; (match Artifact.get_kind artifact with
         | Page _ -> Hidden_targets [ output_file ]
         | Module _ -> Command.Args.empty)
      ]
  in
  (* Add explicit dependency on CSS/support files *)
  let rule =
    let open Action_builder.With_targets.O in
    Action_builder.with_no_targets (Action_builder.path (Path.build odoc_support_path))
    >>>
    match Output_format.dir_target ctx mode output_format artifact with
    | Some dir_target ->
      (* Module: odoc generates a directory tree *)
      Action_builder.With_targets.add_directories
        ~directory_targets:[ dir_target ]
        run_odoc
    | None ->
      (* Page: file target *)
      Action_builder.With_targets.add ~file_targets:[ output_file ] run_odoc
  in
  add_rule sctx rule
;;

let setup_css_rule sctx ~mode =
  let ctx = Super_context.context sctx in
  let dir = Paths.odoc_support ctx mode in
  let run_odoc =
    let cmd =
      run_odoc
        sctx
        "support-files"
        ~quiet:false
        ~flags_for:None
        [ A "-o"; Path (Path.build dir) ]
    in
    Action_builder.With_targets.add_directories ~directory_targets:[ dir ] cmd
  in
  add_rule sctx run_odoc
;;

(* Generate support files for a specific package (when support = per_package).
   Creates both odoc support files (CSS etc.) in the support directory and
   sherlodoc.js in the package's HTML directory. *)
let setup_pkg_support_rule sctx ~mode ~pkg_name =
  let ctx = Super_context.context sctx in
  let support_dir = Paths.odoc_support_for_pkg ctx mode pkg_name in
  let pkg_html_dir = Paths.html_root ctx mode ++ pkg_name in
  let run_odoc =
    let cmd =
      run_odoc
        sctx
        "support-files"
        ~quiet:false
        ~flags_for:None
        [ A "-o"; Path (Path.build support_dir) ]
    in
    Action_builder.With_targets.add_directories ~directory_targets:[ support_dir ] cmd
  in
  add_rule sctx run_odoc
  (* Create sherlodoc.js in the package's HTML directory (not in odoc.support) *)
  >>> Sherlodoc.sherlodoc_dot_js sctx ~dir:pkg_html_dir
;;

(* Compute requires for linking an artifact.
   For modules: use the library's requires + the library itself
   For pages: use all libraries in the package (since pages document the whole package)
   Also includes extra libraries from artifact.extra_libs (pre-resolved from odoc-config.sexp). *)
let compute_link_requires ~artifact =
  let* base_requires =
    match Artifact.get_kind artifact with
    | Module (_, (Lib (_, lib) | Private_lib (_, lib))) ->
      (* Module in a library: use library's transitive dependencies PLUS the library itself.
       This ensures all modules in the library (including hidden/wrapped ones) are compiled
       before any module is linked. Critical for wrapped libraries.
       Use closure to get transitive dependencies, needed for resolving installed library deps. *)
      let* closure = Lib.closure [ lib ] ~linking:false in
      Memo.return
        (Resolve.bind closure ~f:(fun libs ->
           (* Add the library itself to ensure .odoc-all dependency includes all modules *)
           Resolve.return (lib :: libs)))
    | Page ({ pkg_libs; _ }, (Pkg _ | Toplevel _)) ->
      (* Page in a package or toplevel: use the libraries that were recorded when the artifact was created *)
      Memo.return (Resolve.return pkg_libs)
  in
  (* Add extra libraries (pre-resolved from odoc_config at artifact creation) *)
  let extra_libs = Artifact.extra_libs artifact in
  if List.is_empty extra_libs
  then Memo.return base_requires
  else
    Memo.return
      (Resolve.bind base_requires ~f:(fun base_libs ->
         Resolve.return (base_libs @ extra_libs)))
;;

(* Unified linking function that works for all artifact types.
   This follows the driver's pattern where all information needed to link
   is derived from the artifact itself. *)
let link_artifact sctx ~artifact =
  let* requires = compute_link_requires ~artifact in
  (* Call the existing link_odoc_rules with computed requires *)
  link_odoc_rules sctx artifact ~pkg:(Artifact.pkg artifact) ~requires
;;

(* Re-export libs_of_pkg for external use *)
let libs_of_pkg = Odoc_discovery.libs_of_pkg

(* Generate the .mld file for the toplevel index *)
let setup_toplevel_index_mld sctx ~mode =
  let ctx = Super_context.context sctx in
  let* artifact = Odoc_discovery.toplevel_index_artifact ctx ~mode in
  match Artifact.generated_content artifact with
  | Some content ->
    let output_path = Artifact.source_file artifact in
    add_rule
      sctx
      (Action_builder.write_file (Path.as_in_build_dir_exn output_path) content)
  | None -> Memo.return ()
;;

(* Compile the toplevel index .mld to .odoc *)
let setup_toplevel_index_compile sctx ~mode =
  let ctx = Super_context.context sctx in
  let* artifact = Odoc_discovery.toplevel_index_artifact ctx ~mode in
  compile_artifact
    sctx
    ~artifact
    ~lib_artifacts_by_module:Module_name.Map.empty
    ~package_lib_names:Lib_name.Set.empty
;;

(* Link the toplevel index .odoc to .odocl *)
let setup_toplevel_index_link sctx ~mode =
  let ctx = Super_context.context sctx in
  let* artifact = Odoc_discovery.toplevel_index_artifact ctx ~mode in
  link_artifact sctx ~artifact
;;

(* Generate global search database from all visible odocl files *)
let generate_global_search_db sctx ~mode =
  let ctx = Super_context.context sctx in
  let* _real_pkgs, all_odocl_files =
    Odoc_discovery.collect_all_visible_odocls sctx ~mode ()
  in
  let dir = Paths.html_root ctx mode in
  Sherlodoc.search_db sctx ~dir ~external_odocls:[] all_odocl_files
;;

(* Generate HTML for the toplevel index *)
let setup_toplevel_index_html sctx mode =
  let ctx = Super_context.context sctx in
  let* artifact = Odoc_discovery.toplevel_index_artifact ctx ~mode in
  (* Determine sidebar and search scope based on mode and env config *)
  let* flags = Flags.get_memo ~dir:(Context.build_dir ctx) in
  let use_global =
    match flags.sidebar with
    | Flags.Global -> true
    | Flags.Per_package -> false
  in
  (* Create search_db and sidebar only for global mode *)
  let* search_db =
    if use_global
    then
      let+ db = generate_global_search_db sctx ~mode in
      Some db
    else Memo.return None
  in
  let sidebar_file =
    if use_global then Some (Paths.sidebar_file ctx mode Paths.Global) else None
  in
  (* Generate HTML for the artifact (only HTML, not JSON) *)
  generate_html_artifact
    sctx
    ~artifact
    ?search_db
    ~sidebar_file
    ~mode
    ~output_format:Html
    ()
;;

(* Generate JSON for the toplevel index *)
let setup_toplevel_index_json sctx mode =
  let ctx = Super_context.context sctx in
  let* artifact = Odoc_discovery.toplevel_index_artifact ctx ~mode in
  generate_html_artifact sctx ~artifact ~sidebar_file:None ~mode ~output_format:Json ()
;;

(* Add dependencies from the toplevel index alias to all child package aliases.
   This ensures that building @doc-full (or @doc-json) triggers building output for all packages
   and private libraries, including those in vendored directories that are
   skipped by normal alias recursion. *)
let setup_toplevel_index_deps sctx mode output =
  let ctx = Super_context.context sctx in
  let root =
    match output with
    | Output_format.Html -> Paths.html_root ctx mode
    | Output_format.Json -> Paths.json_root ctx mode
  in
  let toplevel_alias = Output_format.alias output ~mode ~dir:root in
  let* items = Odoc_discovery.Toplevel_index.get_items ~mode ctx in
  let child_aliases =
    List.map items ~f:(fun item ->
      let name =
        match item with
        | Odoc_discovery.Toplevel_index.Package { name; _ } -> name
        | Odoc_discovery.Toplevel_index.Private_lib { unique_name; _ } -> unique_name
      in
      let dir = root ++ name in
      Output_format.alias output ~mode ~dir)
  in
  let deps =
    List.map child_aliases ~f:(fun alias -> Dune_engine.Dep.alias alias)
    |> Dune_engine.Dep.Set.of_list
  in
  Rules.Produce.Alias.add_deps toplevel_alias (Action_builder.deps deps)
;;

(* Artifact discovery functions - organized into a module for clarity *)

(* Helper to compute library directory path for private vs package libraries *)
let lib_dir_path ctx ~output ~scope_id ~lib_name =
  let path_prefix =
    match output with
    | Odoc -> "_odoc"
    | Odocls -> "_odocl"
  in
  match scope_id with
  | Scope_id.Private_lib _ ->
    (* Private library: unique_name already identifies the library *)
    Paths.root ctx ++ path_prefix ++ Scope_id.to_string scope_id
  | Scope_id.Package pkg ->
    (* Package library: path is pkg/lib *)
    Paths.root ctx
    ++ path_prefix
    ++ Package.Name.to_string pkg
    ++ Lib_name.to_string lib_name
;;

(* Generate index file from linked .odocl files *)
let generate_index sctx ~mode ~scope ~packages ~odocl_files =
  let ctx = Super_context.context sctx in
  let index_file = Paths.index_file ctx mode scope in
  (* compile-index needs all .odocl files:
     - Library .odocl files
     - Package-level mld .odocl files *)
  let open Command.Args in
  (* Pass all .odocl files as dependencies *)
  let odocl_file_args =
    List.map odocl_files ~f:(fun odocl_file -> Dep (Path.build odocl_file))
  in
  let action =
    let open Action_builder.With_targets.O in
    (* Depend on all packages' .odoc-all aliases *)
    Action_builder.with_no_targets
      (Action_builder.all_unit
         (List.map packages ~f:(fun pkg ->
            let odocl_dir = Paths.odocl ctx (Pkg pkg) in
            let pkg_alias = Dep.odoc_all_alias ~dir:odocl_dir in
            Action_builder.dep (Dune_engine.Dep.alias pkg_alias))))
    >>> run_odoc
          sctx
          "compile-index"
          ~quiet:false
          ~flags_for:None
          ([ A "-o"; Target index_file ] @ odocl_file_args)
  in
  let* () = add_rule sctx action in
  Memo.return index_file
;;

(* Generate binary sidebar file from its index - called in _sidebar handler *)
let generate_sidebar_binary sctx ~mode ~scope ~index_file =
  let ctx = Super_context.context sctx in
  let sidebar_file = Paths.sidebar_file ctx mode scope in
  (* Generate binary sidebar - run from _doc directory with relative path to index *)
  let sidebar_dir =
    match mode with
    | Doc_mode.Local_only -> "_sidebar"
    | Doc_mode.Full -> "_sidebar_full"
  in
  let index_relative_path =
    match scope with
    | Paths.Global -> sprintf "%s/index.odoc-index" sidebar_dir
    | Paths.Per_package pkg ->
      sprintf "%s/%s/index.odoc-index" sidebar_dir (Package.Name.to_string pkg)
  in
  let* () =
    let action =
      let open Action_builder.With_targets.O in
      Action_builder.with_no_targets (Action_builder.path (Path.build index_file))
      >>> run_odoc
            sctx
            "sidebar-generate"
            ~quiet:false
            ~flags_for:None
            [ A "-o"; Target sidebar_file; A index_relative_path ]
    in
    add_rule sctx action
  in
  Memo.return sidebar_file
;;

(* Generate JSON sidebar file from its index *)
let generate_sidebar_json sctx ~mode ~scope ~index_file ~output_format =
  let ctx = Super_context.context sctx in
  let sidebar_json = Paths.sidebar_json ctx mode scope output_format in
  (* Generate JSON sidebar - run from _doc directory with relative path to index *)
  let sidebar_dir =
    match mode with
    | Doc_mode.Local_only -> "_sidebar"
    | Doc_mode.Full -> "_sidebar_full"
  in
  let index_relative_path =
    match scope with
    | Paths.Global -> sprintf "%s/index.odoc-index" sidebar_dir
    | Paths.Per_package pkg ->
      sprintf "%s/%s/index.odoc-index" sidebar_dir (Package.Name.to_string pkg)
  in
  let action =
    let open Action_builder.With_targets.O in
    Action_builder.with_no_targets (Action_builder.path (Path.build index_file))
    >>> run_odoc
          sctx
          "sidebar-generate"
          ~quiet:false
          ~flags_for:None
          [ A "--json"; A "-o"; Target sidebar_json; A index_relative_path ]
  in
  add_rule sctx action
;;

(* Handle sidebar generation for a package or private library *)
let handle_sidebar_artifacts sctx ~mode pkg_or_lib_name =
  let ctx = Super_context.context sctx in
  let rules =
    Rules.collect_unit (fun () ->
      let pkg = Package.Name.of_string pkg_or_lib_name in
      let scope = Paths.Per_package pkg in
      (* Discover artifacts to get all .odocl files *)
      let* all_artifacts, _lib_subdirs =
        Odoc_discovery.discover_package_artifacts
          sctx
          ctx
          ~pkg_or_lib_unique_name:pkg_or_lib_name
      in
      (* Collect all .odocl files from all artifacts (libraries and package pages) *)
      let odocl_files =
        List.filter_map all_artifacts ~f:(fun artifact ->
          if Artifact.hidden artifact
          then None
          else Some (Artifact.odocl_file ctx artifact))
      in
      (* Generate index file with all .odocl files *)
      let* index_file = generate_index sctx ~mode ~scope ~packages:[ pkg ] ~odocl_files in
      (* Generate binary sidebar *)
      let* _sidebar_file = generate_sidebar_binary sctx ~mode ~scope ~index_file in
      Memo.return ())
  in
  Memo.return (Build_config.Gen_rules.make rules)
;;

(* Generate global sidebar for all packages - returns unit Memo.t for use in Rules.collect_unit *)
let generate_global_sidebar sctx ~mode =
  let* real_pkgs, all_odocl_files =
    Odoc_discovery.collect_all_visible_odocls sctx ~mode ()
  in
  (* Generate global index file with all .odocl files *)
  let* index_file =
    generate_index
      sctx
      ~mode
      ~scope:Paths.Global
      ~packages:real_pkgs
      ~odocl_files:all_odocl_files
  in
  (* Generate global binary sidebar *)
  let* _sidebar_file =
    generate_sidebar_binary sctx ~mode ~scope:Paths.Global ~index_file
  in
  Memo.return ()
;;

(* Handle root sidebar directory - conditionally generate global sidebar based on flags *)
let handle_sidebar_root sctx ~dir ~mode =
  let ctx = Super_context.context sctx in
  let* workspace_pkgs = get_workspace_packages () in
  let pkg_subdirs = List.map workspace_pkgs ~f:Package.Name.to_string in
  (* Also include private libraries *)
  let* private_local_libs = Odoc_discovery.get_private_libraries ctx in
  let private_lib_subdirs =
    List.map private_local_libs ~f:(fun local_lib -> Odoc_scope.lib_unique_name local_lib)
  in
  let all_subdirs = pkg_subdirs @ private_lib_subdirs in
  let* flags = Flags.get_memo ~dir:(Context.build_dir ctx) in
  let rules =
    match flags.sidebar with
    | Flags.Global -> Rules.collect_unit (fun () -> generate_global_sidebar sctx ~mode)
    | Flags.Per_package -> Memo.return Rules.empty
  in
  Memo.return
    (Build_config.Gen_rules.make
       ~build_dir_only_sub_dirs:
         (Build_config.Gen_rules.Build_only_sub_dirs.singleton
            ~dir
            (Subdir_set.of_list all_subdirs))
       rules)
;;

(* Handle remap file generation - single file for all external dependencies *)
let handle_remap_artifacts sctx =
  let ctx = Super_context.context sctx in
  let rules =
    Rules.collect_unit (fun () ->
      (* Get all workspace packages *)
      let* workspace_pkgs = get_workspace_packages () in
      (* Collect all private libs from workspace packages (needed for expand_packages_with_odoc_config) *)
      let* private_libs =
        Memo.List.concat_map workspace_pkgs ~f:(fun pkg ->
          let pkg_name = Package.Name.to_string pkg in
          (* Skip synthetic packages (private libs) *)
          let* scope_id = Scope_id.of_string pkg_name in
          match scope_id with
          | Scope_id.Private_lib _ -> Memo.return []
          | Scope_id.Package _ ->
            let* all_artifacts, _lib_subdirs =
              Odoc_discovery.discover_package_artifacts
                sctx
                ctx
                ~pkg_or_lib_unique_name:pkg_name
            in
            Memo.return (List.filter_map all_artifacts ~f:Artifact.lib))
      in
      (* Create package discovery for version lookup *)
      let* pkg_discovery = Package_discovery.create ~context:ctx in
      (* Get all packages in scope (including odoc config deps) *)
      let* all_packages =
        Odoc_discovery.expand_packages_with_odoc_config
          ctx
          ~packages:workspace_pkgs
          ~private_libs
      in
      (* Generate remap mappings for external dependencies *)
      let* mappings = generate_remap_mappings ctx pkg_discovery ~packages:all_packages in
      (* Always create the remap file, even if empty, since it's required as a dependency *)
      let remap_file = Paths.remap_file ctx in
      write_remap_file sctx ~remap_file ~mappings)
  in
  Memo.return (Build_config.Gen_rules.make rules)
;;

(* Helper function to generate HTML/JSON for a package in a specific mode *)
let generate_html_for_package
      sctx
      ~ctx
      ~scope_id
      ~all_artifacts
      ~dir
      ~mode
      ~output_format
      ()
  =
  (* Filter to only visible artifacts for HTML generation *)
  let visible_artifacts =
    List.filter all_artifacts ~f:(fun a -> not (Artifact.hidden a))
  in
  (* Compute sidebar scope based on configuration *)
  let pkg = Scope_id.as_package_name scope_id in
  let* flags = Flags.get_memo ~dir in
  let scope, should_generate_sidebar_json =
    match flags.sidebar with
    | Flags.Global ->
      (* Global sidebar - JSON already generated at root *)
      Paths.Global, false
    | Flags.Per_package ->
      (* Per-package sidebar - generate JSON here *)
      Paths.Per_package pkg, true
  in
  let index_file = Paths.index_file ctx mode scope in
  (* Generate sidebar.json for the appropriate output directory *)
  let paths_output_format =
    match output_format with
    | Output_format.Html -> Paths.Html
    | Output_format.Json -> Paths.Json
  in
  let* () =
    if should_generate_sidebar_json
    then
      generate_sidebar_json
        sctx
        ~mode
        ~scope
        ~index_file
        ~output_format:paths_output_format
    else Memo.return ()
  in
  (* Reference binary sidebar for HTML generation only *)
  let sidebar_file_opt =
    match output_format with
    | Html -> Some (Paths.sidebar_file ctx mode scope)
    | Json -> None
  in
  (* Create search_db - only for HTML *)
  let* search_db =
    match output_format with
    | Json -> Memo.return None
    | Html ->
      (match flags.sidebar with
       | Flags.Global ->
         (* Global sidebar mode - reference the global search db at html root.
           The rule is already added by setup_toplevel_index_html. *)
         let html_root = Paths.html_root ctx mode in
         Memo.return (Some (Path.Build.relative html_root "db.js"))
       | Flags.Per_package ->
         (* Per-package mode - use package's own search db *)
         let odocls =
           List.map visible_artifacts ~f:(fun artifact ->
             Artifact.odocl_file ctx artifact)
         in
         let+ db = Sherlodoc.search_db sctx ~dir ~external_odocls:[] odocls in
         Some db)
  in
  (* Use shared remap file for Local_only mode *)
  let remap_file_opt =
    match mode with
    | Doc_mode.Local_only -> Some (Paths.remap_file ctx)
    | Doc_mode.Full -> None
  in
  (* Generate output for all visible artifacts *)
  let pkg_name = Scope_id.to_string scope_id in
  let* () =
    Memo.parallel_iter visible_artifacts ~f:(fun artifact ->
      match remap_file_opt with
      | None ->
        generate_html_artifact
          sctx
          ~artifact
          ?search_db
          ~sidebar_file:sidebar_file_opt
          ~mode
          ~output_format
          ~pkg_name
          ()
      | Some rf ->
        generate_html_artifact
          sctx
          ~artifact
          ?search_db
          ~sidebar_file:sidebar_file_opt
          ~remap_file:(Some rf)
          ~mode
          ~output_format
          ~pkg_name
          ())
  in
  (* Create package-level alias with all output files *)
  let artifact_paths =
    List.map visible_artifacts ~f:(fun artifact ->
      Path.build (Output_format.target ctx mode output_format artifact))
  in
  (* Also include sidebar.json if we generated it *)
  let all_paths =
    if should_generate_sidebar_json
    then (
      let sidebar_json = Paths.sidebar_json ctx mode scope paths_output_format in
      Path.build sidebar_json :: artifact_paths)
    else artifact_paths
  in
  let pkg_alias = Dep.format_alias output_format mode ctx (Pkg pkg) in
  let* () = Dep.add_file_deps pkg_alias all_paths in
  (* Also create library-level aliases for each library *)
  let visible_lib_artifacts =
    List.filter visible_artifacts ~f:(fun a ->
      match Artifact.get_kind a with
      | Module _ -> true
      | Page _ -> false)
  in
  (* Add each visible library artifact's files to its library alias *)
  let* () =
    Memo.parallel_iter visible_lib_artifacts ~f:(fun artifact ->
      match Artifact.get_kind artifact with
      | Module (_, ((Lib (_, _) | Private_lib (_, _)) as target)) ->
        let lib_alias = Dep.format_alias output_format mode ctx target in
        let output_file =
          Path.build (Output_format.target ctx mode output_format artifact)
        in
        Dep.add_file_deps lib_alias [ output_file ]
      | Page _ -> Memo.return () (* Package artifacts don't have library aliases *))
  in
  Memo.return ()
;;

(* Common setup for package artifact handlers: discovers artifacts and computes lib names *)
let with_package_artifacts sctx ~dir ~pkg_or_lib_name ~f =
  let ctx = Super_context.context sctx in
  let* scope_id = Scope_id.of_string pkg_or_lib_name in
  let* all_artifacts, lib_subdirs =
    Odoc_discovery.discover_package_artifacts
      sctx
      ctx
      ~pkg_or_lib_unique_name:pkg_or_lib_name
  in
  let all_lib_names =
    List.map lib_subdirs ~f:Lib_name.of_string |> Lib_name.Set.of_list
  in
  let+ rules = f ~ctx ~scope_id ~all_artifacts ~all_lib_names in
  Build_config.Gen_rules.make
    ~build_dir_only_sub_dirs:
      (Build_config.Gen_rules.Build_only_sub_dirs.singleton
         ~dir
         (Subdir_set.of_list lib_subdirs))
    rules
;;

let handle_odoc_artifacts sctx ~dir ~pkg_or_lib_name =
  Log.info [ Pp.textf "handle_odoc_artifacts: %s" pkg_or_lib_name ];
  with_package_artifacts
    sctx
    ~dir
    ~pkg_or_lib_name
    ~f:(fun ~ctx ~scope_id ~all_artifacts ~all_lib_names ->
      Log.info
        [ Pp.textf
            "handle_odoc_artifacts(%s): %d artifacts, %d libs: %s"
            pkg_or_lib_name
            (List.length all_artifacts)
            (Lib_name.Set.cardinal all_lib_names)
            (Lib_name.Set.to_list all_lib_names
             |> List.map ~f:Lib_name.to_string
             |> String.concat ~sep:", ")
        ];
      Memo.return
        (Rules.collect_unit (fun () ->
           (* Build map from module name to odoc path once for all artifacts. *)
           let lib_artifacts_by_module =
             List.fold_left
               all_artifacts
               ~init:Module_name.Map.empty
               ~f:(fun acc artifact ->
                 match Artifact.get_kind artifact with
                 | Module ({ module_name; _ }, _) ->
                   Module_name.Map.set
                     acc
                     module_name
                     (Path.build (Artifact.odoc_file ctx artifact))
                 | Page _ -> acc)
           in
           let* () =
             Memo.parallel_iter all_artifacts ~f:(fun artifact ->
               compile_artifact
                 sctx
                 ~artifact
                 ~lib_artifacts_by_module
                 ~package_lib_names:all_lib_names)
           in
           let* () =
             Memo.parallel_iter all_artifacts ~f:(fun artifact ->
               let odoc_file = Path.build (Artifact.odoc_file ctx artifact) in
               match Artifact.get_kind artifact with
               | Module (_, target) ->
                 Dep.setup_deps ctx target (Path.Set.singleton odoc_file)
               | Page (_, target) ->
                 Dep.setup_deps ctx target (Path.Set.singleton odoc_file))
           in
           let lib_names_with_artifacts =
             List.filter_map all_artifacts ~f:(fun a ->
               Option.map (Artifact.lib a) ~f:Lib.name)
             |> Lib_name.Set.of_list
           in
           let* lib_alias_dirs =
             Lib_name.Set.to_list all_lib_names
             |> Memo.List.filter_map ~f:(fun lib_name ->
               if Lib_name.Set.mem lib_names_with_artifacts lib_name
               then Memo.return (Some (lib_dir_path ctx ~output:Odoc ~scope_id ~lib_name))
               else (
                 let lib_dir = lib_dir_path ctx ~output:Odoc ~scope_id ~lib_name in
                 let alias = Dep.odoc_all_alias ~dir:lib_dir in
                 let+ () = Dep.add_file_deps alias [] in
                 Some lib_dir))
           in
           match scope_id with
           | Scope_id.Private_lib _ -> Memo.return ()
           | Scope_id.Package pkg ->
             let pkg_dir = Paths.root ctx ++ "_odoc" ++ Package.Name.to_string pkg in
             let pkg_alias = Dep.odoc_all_alias ~dir:pkg_dir in
             Dep.add_odoc_all_deps pkg_alias ~dirs:lib_alias_dirs)))
;;

let handle_odocl_artifacts sctx ~dir ~pkg_or_lib_name =
  with_package_artifacts
    sctx
    ~dir
    ~pkg_or_lib_name
    ~f:(fun ~ctx ~scope_id ~all_artifacts ~all_lib_names ->
      Memo.return
        (Rules.collect_unit (fun () ->
           let visible_artifacts =
             List.filter all_artifacts ~f:(fun a -> not (Artifact.hidden a))
           in
           let* () =
             Memo.parallel_iter visible_artifacts ~f:(fun artifact ->
               link_artifact sctx ~artifact)
           in
           let visible_lib_artifacts =
             List.filter visible_artifacts ~f:(fun a ->
               match Artifact.get_kind a with
               | Module _ -> true
               | Page _ -> false)
           in
           let* () =
             Memo.parallel_iter visible_lib_artifacts ~f:(fun artifact ->
               match Artifact.lib artifact with
               | Some lib ->
                 let lib_name = Lib.name lib in
                 let lib_dir = lib_dir_path ctx ~output:Odocls ~scope_id ~lib_name in
                 let lib_alias = Dep.odoc_all_alias ~dir:lib_dir in
                 let odocl_file = Path.build (Artifact.odocl_file ctx artifact) in
                 Dep.add_file_deps lib_alias [ odocl_file ]
               | None -> Memo.return ())
           in
           let lib_names_with_artifacts =
             List.filter_map visible_lib_artifacts ~f:(fun a ->
               Option.map (Artifact.lib a) ~f:Lib.name)
             |> Lib_name.Set.of_list
           in
           let* () =
             Lib_name.Set.to_list all_lib_names
             |> Memo.parallel_iter ~f:(fun lib_name ->
               if Lib_name.Set.mem lib_names_with_artifacts lib_name
               then Memo.return ()
               else (
                 let lib_dir = lib_dir_path ctx ~output:Odocls ~scope_id ~lib_name in
                 let lib_alias = Dep.odoc_all_alias ~dir:lib_dir in
                 Dep.add_file_deps lib_alias []))
           in
           match scope_id with
           | Scope_id.Private_lib _ -> Memo.return ()
           | Scope_id.Package pkg ->
             let pkg_dir = Paths.odocl_root ctx ++ Package.Name.to_string pkg in
             let pkg_alias = Dep.odoc_all_alias ~dir:pkg_dir in
             let all_odocl_paths =
               List.map visible_artifacts ~f:(fun a ->
                 Path.build (Artifact.odocl_file ctx a))
             in
             Dep.add_file_deps pkg_alias all_odocl_paths)))
;;

let handle_output_artifacts sctx ~dir ~mode ~pkg_or_lib_name ~output_format =
  let ctx = Super_context.context sctx in
  let* scope_id = Scope_id.of_string pkg_or_lib_name in
  let* all_artifacts, lib_subdirs =
    Odoc_discovery.discover_package_artifacts
      sctx
      ctx
      ~pkg_or_lib_unique_name:pkg_or_lib_name
  in
  let all_lib_names =
    List.map lib_subdirs ~f:Lib_name.of_string |> Lib_name.Set.of_list
  in
  (* Check if we need per-package support files (HTML only) *)
  let* flags = Flags.get_memo ~dir:(Context.build_dir ctx) in
  let needs_pkg_support =
    match flags.support, output_format with
    | Flags.Per_package, Output_format.Html -> true
    | _ -> false
  in
  (* Collect directory targets for module directories (deduplicated) *)
  let module_dir_targets =
    List.filter_map all_artifacts ~f:(fun artifact ->
      if Artifact.hidden artifact
      then None
      else Output_format.dir_target ctx mode output_format artifact)
    |> Path.Build.Set.of_list
    |> Path.Build.Set.to_list
  in
  (* Add support directory target if per-package support is enabled *)
  let all_dir_targets =
    if needs_pkg_support
    then (
      let support_dir = Paths.odoc_support_for_pkg ctx mode pkg_or_lib_name in
      support_dir :: module_dir_targets)
    else module_dir_targets
  in
  let directory_targets =
    List.map all_dir_targets ~f:(fun dir -> dir, Loc.none) |> Path.Build.Map.of_list_exn
  in
  (* Note: we don't add odoc.support to subdirs - it's just a directory target,
     same as at the root level. Adding it to subdirs causes conflicts. *)
  let other_format = Output_format.other output_format in
  let rules =
    Rules.collect_unit (fun () ->
      (* Generate per-package support files if needed *)
      (if needs_pkg_support
       then setup_pkg_support_rule sctx ~mode ~pkg_name:pkg_or_lib_name
       else Memo.return ())
      >>> generate_html_for_package
            sctx
            ~ctx
            ~scope_id
            ~all_artifacts
            ~dir
            ~mode
            ~output_format
            ()
      (* Also define empty alias for the other format to prevent alias recursion issues *)
      >>>
      let other_alias = Output_format.alias other_format ~mode ~dir in
      Rules.Produce.Alias.add_deps other_alias (Action_builder.return ())
      >>>
      (* Add empty aliases for the other format in library subdirectories too *)
      Memo.parallel_iter (Lib_name.Set.to_list all_lib_names) ~f:(fun lib_name ->
        let lib_dir = dir ++ Lib_name.to_string lib_name in
        let lib_other_alias = Output_format.alias other_format ~mode ~dir:lib_dir in
        Rules.Produce.Alias.add_deps lib_other_alias (Action_builder.return ())))
  in
  Memo.return
    (Build_config.Gen_rules.make
       ~build_dir_only_sub_dirs:
         (Build_config.Gen_rules.Build_only_sub_dirs.singleton
            ~dir
            (Subdir_set.of_list lib_subdirs))
       ~directory_targets
       rules)
;;

let handle_html_artifacts sctx ~dir ~mode ~pkg_or_lib_name =
  handle_output_artifacts sctx ~dir ~mode ~pkg_or_lib_name ~output_format:Html
;;

let handle_json_artifacts sctx ~dir ~mode ~pkg_or_lib_name =
  handle_output_artifacts sctx ~dir ~mode ~pkg_or_lib_name ~output_format:Json
;;

let setup_package_aliases_format
      sctx
      (pkg : Package.t)
      (output : Output_format.t)
      (mode : Doc_mode.t)
  =
  let ctx = Super_context.context sctx in
  let name = Package.name pkg in
  let alias =
    let pkg_dir = Package.dir pkg in
    let dir = Path.Build.append_source (Context.build_dir ctx) pkg_dir in
    Output_format.alias output ~mode ~dir
  in
  (* Wrap the entire transitive closure computation in Action_builder. *)
  let deps_action =
    let open Action_builder.O in
    let* dep_set =
      Action_builder.of_memo
        (let open Memo.O in
         (* Get doc dependency packages from Package_info.documentation *)
         let doc = Package.info pkg |> Package_info.documentation in
         let doc_dep_packages =
           List.map doc.packages ~f:(fun (dep : Package_dependency.t) -> dep.name)
         in
         (* Use expand_packages_with_odoc_config to get all required packages *)
         let* all_packages =
           Odoc_discovery.expand_packages_with_odoc_config
             ctx
             ~packages:(name :: doc_dep_packages)
             ~private_libs:[]
         in
         (* Convert packages to targets *)
         let pkg_targets =
           Package.Name.Set.to_list all_packages
           |> List.map ~f:(fun p -> Target.Any (Target.Pkg p))
         in
         (* Add toplevel target *)
         let all_targets = pkg_targets @ [ Target.Any (Target.Toplevel mode) ] in
         (* Filter based on mode: Local_only includes only workspace packages *)
         let* filtered_targets =
           match mode with
           | Doc_mode.Local_only ->
             let* workspace_pkgs = get_workspace_packages () in
             let workspace_pkg_set = Package.Name.Set.of_list workspace_pkgs in
             Memo.return
               (List.filter all_targets ~f:(fun (Target.Any target) ->
                  match target with
                  | Target.Pkg p -> Package.Name.Set.mem workspace_pkg_set p
                  | Target.Lib _ | Target.Private_lib _ -> true
                  | Target.Toplevel _ -> true))
           | Doc_mode.Full ->
             (* Include all dependencies *)
             Memo.return all_targets
         in
         let unique_targets =
           List.sort_uniq filtered_targets ~compare:Target.compare_any
         in
         Memo.return
           (unique_targets
            |> List.map ~f:(fun (Target.Any t) -> Dep.format_alias output mode ctx t)
            |> Dune_engine.Dep.Set.of_list_map ~f:(fun f -> Dune_engine.Dep.alias f)))
    in
    let* dep_set_with_remap =
      match mode with
      | Doc_mode.Local_only ->
        (* Add remap file as dependency for Local_only mode *)
        let remap_file = Paths.remap_file ctx in
        let+ _ = Action_builder.path (Path.build remap_file) in
        dep_set
      | Doc_mode.Full -> Action_builder.return dep_set
    in
    Action_builder.deps dep_set_with_remap
  in
  Rules.Produce.Alias.add_deps alias deps_action
;;

let setup_package_aliases sctx (pkg : Package.t) =
  (* Set up aliases for both modes *)
  Memo.List.iter Doc_mode.all ~f:(fun mode ->
    Memo.parallel_iter Output_format.all ~f:(fun output ->
      setup_package_aliases_format sctx pkg output mode))
;;

(* setup_package_odoc_rules removed - unused old function *)

(* ============================================================================
   HIGH-LEVEL - Entry points and rule generation
   ============================================================================ *)

let gen_project_rules sctx project =
  let* mask = Dune_load.mask () in
  (* Set up package aliases *)
  Dune_project.packages project
  |> Dune_lang.Package_name.Map.to_seq
  |> Memo.parallel_iter_seq ~f:(fun (_, (pkg : Package.t)) ->
    (* Check if this package is in the mask (honors -p flag) *)
    let should_build =
      Only_packages.mem_all mask || Only_packages.mem mask (Package.name pkg)
    in
    if should_build
    then
      (* setup @doc to build the correct html for the package *)
      setup_package_aliases sctx pkg
    else Memo.return ())
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
    (* Create target for this private library and add its HTML to doc-private and doc-full.
       Dependencies are handled transitively through the odoc pipeline. *)
    let local_lib = Lib.Local.of_lib_exn lib in
    let lib_unique_name = Odoc_scope.lib_unique_name local_lib in
    let target = Target.Private_lib (lib_unique_name, lib) in
    let html_alias_local = Dep.format_alias Html Doc_mode.Local_only ctx target in
    let html_alias_full = Dep.format_alias Html Doc_mode.Full ctx target in
    let alias_dep alias =
      Action_builder.deps (Dune_engine.Dep.Set.singleton (Dune_engine.Dep.alias alias))
    in
    Rules.Produce.Alias.add_deps
      (Alias.make ~dir Alias0.private_doc)
      (alias_dep html_alias_local)
    >>> Rules.Produce.Alias.add_deps
          (Alias.make ~dir Alias0.doc_full)
          (alias_dep html_alias_full)
;;

let has_rules ?(directory_targets = Path.Build.Map.empty) f =
  let rules = Rules.collect_unit f in
  Memo.return (Gen_rules.make ~directory_targets rules)
;;

let handle_classify_dir sctx ~pkg_name ~lib_name =
  (* classify library directory: _doc/classify/{package}/{library} *)
  let pkg = Package.Name.of_string pkg_name in
  let lib_name = Lib_name.of_string lib_name in
  let ctx = Super_context.context sctx in
  (* Find the library by name within the package *)
  let* pkg_libs = libs_of_pkg ctx ~pkg in
  let* lib_opt =
    Memo.List.find_map pkg_libs ~f:(fun lib ->
      if Lib_name.equal (Lib.name lib) lib_name
      then Memo.return (Some lib)
      else Memo.return None)
  in
  match lib_opt with
  | None -> Memo.return ()
  | Some lib ->
    (* Only generate classify for installed libraries, not local ones *)
    (match Lib.Local.of_lib lib with
     | Some _ -> Memo.return ()
     | None ->
       let info = Lib.info lib in
       let src_dir = Lib_info.src_dir info in
       let classify_output =
         Paths.root ctx
         ++ "classify"
         ++ pkg_name
         ++ Lib_name.to_string lib_name
         ++ "odoc.classify"
       in
       let run_classify =
         let program = odoc_program sctx (Context.build_dir ctx) in
         let deps = Action_builder.env_var "ODOC_SYNTAX" in
         let open Action_builder.With_targets.O in
         Action_builder.with_no_targets deps
         >>> Command.run_dyn_prog
               ~dir:(Path.build (Context.build_dir ctx))
               ~stdout_to:classify_output
               program
               [ A "classify"; A (Path.to_string src_dir) ]
       in
       add_rule sctx run_classify)
;;

let handle_mlds_dir sctx ~pkg_name =
  (* _mlds/{pkg} - Generate mld files for package *)
  let ctx = Super_context.context sctx in
  let* all_artifacts, _lib_subdirs =
    Odoc_discovery.discover_package_artifacts sctx ctx ~pkg_or_lib_unique_name:pkg_name
  in
  (* Filter for Generated artifacts and create write rules for them *)
  Memo.List.iter all_artifacts ~f:(fun artifact ->
    match Artifact.generated_content artifact with
    | None -> Memo.return ()
    | Some content ->
      let output_path = Artifact.source_file artifact in
      add_rule
        sctx
        (Action_builder.write_file (Path.as_in_build_dir_exn output_path) content))
;;

(* Handle root output directories (_html, _html_full, _json, _json_full) *)
let handle_output_root sctx ~mode ~output_format =
  let ctx = Super_context.context sctx in
  let directory_targets =
    match output_format with
    | Output_format.Html ->
      Path.Build.Map.singleton (Paths.odoc_support ctx mode) Loc.none
    | Output_format.Json -> Path.Build.Map.empty
  in
  let rules =
    Rules.collect_unit (fun () ->
      (* HTML-specific setup: sherlodoc and CSS *)
      (match output_format with
       | Output_format.Html ->
         Sherlodoc.sherlodoc_dot_js sctx ~dir:(Paths.html_root ctx mode)
         >>> setup_css_rule sctx ~mode
         >>> setup_toplevel_index_html sctx mode
       | Output_format.Json -> setup_toplevel_index_json sctx mode)
      >>> let* artifact = Odoc_discovery.toplevel_index_artifact ctx ~mode in
          let output_file = Output_format.target ctx mode output_format artifact in
          let alias = Dep.format_alias output_format mode ctx (Toplevel mode) in
          Dep.add_file_deps alias [ Path.build output_file ]
          >>>
          (* Generate global sidebar JSON if configured *)
          let* flags = Flags.get_memo ~dir:(Context.build_dir ctx) in
          (match flags.sidebar with
           | Flags.Global ->
             let index_file = Paths.index_file ctx mode Paths.Global in
             generate_sidebar_json
               sctx
               ~mode
               ~scope:Paths.Global
               ~index_file
               ~output_format:(Output_format.to_paths_format output_format)
           | Flags.Per_package -> Memo.return ())
          (* Add dependencies on all child directories so the alias builds everything *)
          >>> setup_toplevel_index_deps sctx mode output_format)
  in
  Memo.return (Build_config.Gen_rules.make ~directory_targets rules)
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
    handle_output_root sctx ~mode:Doc_mode.Local_only ~output_format:Output_format.Html
  | [ "_html"; _; _ ] | [ "_html"; _; _; _ ] ->
    (* Library/module directories redirect to parent - package level handler generates HTML *)
    Memo.return (Gen_rules.redirect_to_parent Gen_rules.Rules.empty)
  | [ "_mlds" ] ->
    (* Root _mlds directory - just allows any package subdirectory (toplevel mld is in _index) *)
    Memo.return (Build_config.Gen_rules.make (Memo.return Rules.empty))
  | [ "_mlds"; pkg_name ] ->
    (* Package mlds directory - generate mld files and allow library subdirs *)
    let pkg = Package.Name.of_string pkg_name in
    let ctx = Super_context.context sctx in
    (* libs_of_pkg already returns appropriate libs based on local vs installed *)
    let* all_libs = libs_of_pkg ctx ~pkg in
    let lib_subdirs =
      List.map all_libs ~f:(fun lib -> Lib.name lib |> Lib_name.to_string)
    in
    let rules = Rules.collect_unit (fun () -> handle_mlds_dir sctx ~pkg_name) in
    Memo.return
      (Build_config.Gen_rules.make
         ~build_dir_only_sub_dirs:
           (Build_config.Gen_rules.Build_only_sub_dirs.singleton
              ~dir
              (Subdir_set.of_list lib_subdirs))
         rules)
  | [ "_mlds"; _; _ ] ->
    (* Library mld directories redirect to parent *)
    Memo.return (Gen_rules.redirect_to_parent Gen_rules.Rules.empty)
  | [ "_odoc" ] ->
    (* Root odoc directory - just allows any package subdirectory *)
    Memo.return (Build_config.Gen_rules.make (Memo.return Rules.empty))
  | [ "_odoc"; pkg_or_lib_name ] -> handle_odoc_artifacts sctx ~dir ~pkg_or_lib_name
  | [ "_odoc"; _; _ ] ->
    (* Library directories redirect to parent *)
    Memo.return (Gen_rules.redirect_to_parent Gen_rules.Rules.empty)
  | [ "_odocl" ] ->
    (* Root odocl directory - just allows any package subdirectory *)
    Memo.return (Build_config.Gen_rules.make (Memo.return Rules.empty))
  | [ "_odocl"; pkg_or_lib_name ] -> handle_odocl_artifacts sctx ~dir ~pkg_or_lib_name
  | [ "_odocl"; _; _ ] ->
    (* Library directories redirect to parent *)
    Memo.return (Gen_rules.redirect_to_parent Gen_rules.Rules.empty)
  | [ "_index" ] ->
    (* Toplevel index directory for Local_only mode - contains mld, odoc, and odocl *)
    let rules =
      Rules.collect_unit (fun () ->
        setup_toplevel_index_mld sctx ~mode:Doc_mode.Local_only
        >>> setup_toplevel_index_compile sctx ~mode:Doc_mode.Local_only
        >>> setup_toplevel_index_link sctx ~mode:Doc_mode.Local_only)
    in
    Memo.return (Build_config.Gen_rules.make rules)
  | [ "_index_full" ] ->
    (* Toplevel index directory for Full mode - contains mld, odoc, and odocl *)
    let rules =
      Rules.collect_unit (fun () ->
        setup_toplevel_index_mld sctx ~mode:Doc_mode.Full
        >>> setup_toplevel_index_compile sctx ~mode:Doc_mode.Full
        >>> setup_toplevel_index_link sctx ~mode:Doc_mode.Full)
    in
    Memo.return (Build_config.Gen_rules.make rules)
  | [ "_html"; pkg_or_lib_name ] ->
    handle_html_artifacts sctx ~dir ~mode:Doc_mode.Local_only ~pkg_or_lib_name
  | [ "_html_full" ] ->
    handle_output_root sctx ~mode:Doc_mode.Full ~output_format:Output_format.Html
  | [ "_html_full"; pkg_or_lib_name ] ->
    handle_html_artifacts sctx ~dir ~mode:Doc_mode.Full ~pkg_or_lib_name
  | [ "_html_full"; _; _ ] | [ "_html_full"; _; _; _ ] ->
    (* Library/module directories redirect to parent - package level handler generates HTML *)
    Memo.return (Gen_rules.redirect_to_parent Gen_rules.Rules.empty)
  | [ "_json" ] ->
    handle_output_root sctx ~mode:Doc_mode.Local_only ~output_format:Output_format.Json
  | [ "_json"; pkg_or_lib_name ] ->
    handle_json_artifacts sctx ~dir ~mode:Doc_mode.Local_only ~pkg_or_lib_name
  | [ "_json"; _; _ ] | [ "_json"; _; _; _ ] ->
    (* JSON subdirectories redirect to parent *)
    Memo.return (Gen_rules.redirect_to_parent Gen_rules.Rules.empty)
  | [ "_json_full" ] ->
    handle_output_root sctx ~mode:Doc_mode.Full ~output_format:Output_format.Json
  | [ "_json_full"; pkg_or_lib_name ] ->
    handle_json_artifacts sctx ~dir ~mode:Doc_mode.Full ~pkg_or_lib_name
  | [ "_json_full"; _; _ ] | [ "_json_full"; _; _; _ ] ->
    (* JSON subdirectories redirect to parent *)
    Memo.return (Gen_rules.redirect_to_parent Gen_rules.Rules.empty)
  | [ "_sidebar" ] -> handle_sidebar_root sctx ~dir ~mode:Doc_mode.Local_only
  | [ "_sidebar"; pkg_or_lib_name ] ->
    handle_sidebar_artifacts sctx ~mode:Doc_mode.Local_only pkg_or_lib_name
  | [ "_sidebar_full" ] -> handle_sidebar_root sctx ~dir ~mode:Doc_mode.Full
  | [ "_sidebar_full"; pkg_or_lib_name ] ->
    handle_sidebar_artifacts sctx ~mode:Doc_mode.Full pkg_or_lib_name
  | [ "_remap" ] ->
    (* Remap directory - generate single remap file for all external dependencies *)
    handle_remap_artifacts sctx
  | [ "_sherlodoc" ] ->
    (* Sherlodoc directory - generate global marshal database for all odocl files including deps *)
    let ctx = Super_context.context sctx in
    let rules =
      Rules.collect_unit (fun () ->
        let* _real_pkgs, all_odocl_files =
          Odoc_discovery.collect_all_visible_odocls sctx ~mode:Doc_mode.Full ()
        in
        let dir = Paths.sherlodoc_root ctx in
        let+ _db =
          Sherlodoc.search_db_marshal sctx ~dir ~external_odocls:[] all_odocl_files
        in
        ())
    in
    Memo.return (Gen_rules.make rules)
  | [ "classify"; pkg_name; lib_name ] ->
    has_rules (fun () -> handle_classify_dir sctx ~pkg_name ~lib_name)
  | _ ->
    (* Unmatched paths get empty rules with no subdirectories allowed *)
    Memo.return (Build_config.Gen_rules.make (Memo.return Rules.empty))
;;
