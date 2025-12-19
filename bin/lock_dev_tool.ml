open Dune_config
open Import
module Lock_dir = Dune_pkg.Lock_dir
module Pin = Dune_pkg.Pin

let is_enabled =
  lazy
    (match Config.get Dune_rules.Compile_time.lock_dev_tools with
     | `Enabled -> true
     | `Disabled -> false)
;;

(* Returns a version constraint accepting (almost) all versions whose prefix is
   the given version. This allows alternative distributions of packages to be
   chosen, such as choosing "ocamlformat.0.26.2+binary" when .ocamlformat
   contains "version=0.26.2". *)
let relaxed_version_constraint_of_version version =
  let open Dune_lang in
  let min_version = Package_version.to_string version in
  (* The goal here is to add a suffix to [min_version] to construct a version
     number higher than than any version number likely to appear with
     [min_version] as a prefix. "_" is the highest ascii symbol that can appear
     in version numbers, excluding "~" which has a special meaning. It's
     conceivable that one or two consecutive "_" characters may be used in a
     version, so this appends "___" to [min_version].

     Read more at: https://opam.ocaml.org/doc/Manual.html#Version-ordering
  *)
  let max_version = min_version ^ "___MAX_VERSION" in
  Package_constraint.And
    [ Package_constraint.Uop
        (Relop.Gte, Package_constraint.Value.String_literal min_version)
    ; Package_constraint.Uop
        (Relop.Lte, Package_constraint.Value.String_literal max_version)
    ]
;;

(* The solver satisfies dependencies for local packages, but dev tools
   are not local packages. As a workaround, create an empty local package
   which depends on the dev tool package. *)
let make_local_package_wrapping_dev_tool ~dev_tool ~dev_tool_version ~extra_dependencies
  : Dune_pkg.Local_package.t
  =
  let dev_tool_pkg_name = Dune_pkg.Dev_tool.package_name dev_tool in
  let dependency =
    let open Dune_lang in
    let open Package_dependency in
    let constraint_ =
      Option.map dev_tool_version ~f:relaxed_version_constraint_of_version
    in
    { name = dev_tool_pkg_name; constraint_ }
  in
  let local_package_name =
    Package_name.of_string (Package_name.to_string dev_tool_pkg_name ^ "_dev_tool_wrapper")
  in
  { Dune_pkg.Local_package.name = local_package_name
  ; version = Dune_pkg.Lock_dir.Pkg_info.default_version
  ; dependencies =
      Dune_pkg.Dependency_formula.of_dependencies (dependency :: extra_dependencies)
  ; conflicts = []
  ; depopts = []
  ; pins = Package_name.Map.empty
  ; conflict_class = []
  ; loc = Loc.none
  ; command_source = Opam_file { build = []; install = [] }
  }
;;

let solve ~dev_tool ~local_packages =
  let open Memo.O in
  let* solver_env_from_current_system =
    Pkg.Pkg_common.poll_solver_env_from_current_system ()
    |> Memo.of_reproducible_fiber
    >>| Option.some
  and* workspace =
    let+ workspace = Workspace.workspace () in
    match Config.get Dune_rules.Compile_time.bin_dev_tools with
    | `Enabled ->
      Workspace.add_repo workspace Dune_pkg.Pkg_workspace.Repository.binary_packages
    | `Disabled -> workspace
  in
  (* as we want to write to the source, we're using the source lock dir here *)
  let lock_dir =
    Dune_rules.Lock_dir.dev_tool_external_lock_dir dev_tool |> Path.external_
  in
  Memo.of_reproducible_fiber
  @@ Pkg.Lock.solve
       workspace
       ~local_packages
       ~project_pins:Pin.DB.empty
       ~solver_env_from_current_system
       ~version_preference:None
       ~lock_dirs:[ lock_dir ]
       ~print_perf_stats:false
       ~portable_lock_dir:false
;;

(* Some dev tools must be built with the same version of the ocaml compiler as
   the project. This function returns the compiler package used to compile the
   project in the default build context.

   TODO: This only deduces version constraints of the compiler, however this
   won't work for custom compiler pins. *)

let compiler_package () =
  let open Memo.O in
  let context = Context_name.default in
  let* result = Dune_rules.Lock_dir.get context
  and* platform =
    Pkg.Pkg_common.poll_solver_env_from_current_system () |> Memo.of_reproducible_fiber
  in
  match result with
  | Error _ ->
    User_error.raise
      [ Pp.text "Unable to load the lockdir for the default build context." ]
      ~hints:
        [ Pp.concat
            ~sep:Pp.space
            [ Pp.text "Try running"; User_message.command "dune build" ]
        ]
  | Ok lockfile ->
    let pkgs = Lock_dir.Packages.pkgs_on_platform_by_name lockfile.packages ~platform in
    (match lockfile.ocaml with
     | None ->
       User_error.raise
         [ Pp.textf "No compiler declared in the lockfile" ]
         ~hints:
           [ Pp.concat
               ~sep:Pp.space
               [ Pp.text
                   "Add a dependency on a compiler to one of the packages in \
                    dune-project and then run"
               ; User_message.command "dune build"
               ]
           ]
     | Some (_loc, pkg_name) ->
       (match Package_name.Map.find pkgs pkg_name with
        | None ->
          User_error.raise
            [ Pp.textf "Compiler package %s not found." (Package_name.to_string pkg_name)
            ]
        | Some pkg -> Memo.return pkg))
;;

let compiler_constraints () =
  let open Memo.O in
  let open Dune_lang in
  let+ pkg = compiler_package () in
  let version = pkg.info.version in
  let constraint_ =
    Some (Package_constraint.Uop (Eq, String_literal (Package_version.to_string version)))
  in
  [ { Package_dependency.name = pkg.info.name; constraint_ } ]
;;

let extra_dependencies dev_tool =
  match Dune_pkg.Dev_tool.needs_to_build_with_same_compiler_as_project dev_tool with
  | false -> Memo.return []
  | true -> compiler_constraints ()
;;

let lockdir_status dev_tool =
  let open Memo.O in
  let dev_tool_lock_dir = Dune_rules.Lock_dir.dev_tool_external_lock_dir dev_tool in
  let* lock_dir_exists =
    Dune_engine.Fs_memo.dir_exists (Path.Outside_build_dir.External dev_tool_lock_dir)
  in
  match lock_dir_exists with
  | false -> Memo.return `No_lockdir
  | true ->
    let dev_tool_lock_dir = Path.external_ dev_tool_lock_dir in
    (match Lock_dir.read_disk dev_tool_lock_dir with
     | Error _ -> Memo.return `No_lockdir
     | Ok { packages; _ } ->
       let* platform =
         Pkg.Pkg_common.poll_solver_env_from_current_system ()
         |> Memo.of_reproducible_fiber
       in
       let packages = Lock_dir.Packages.pkgs_on_platform_by_name packages ~platform in
       let package_name = Dune_pkg.Dev_tool.package_name dev_tool in
       (match Package_name.Map.find packages package_name with
        | None ->
          Memo.return
            (`Lockdir_missing_entry_for_tool
                (User_message.make
                   [ Pp.textf
                       "The lock directory for the tool %S exists but does not contain a \
                        lockfile for the package %S. This may indicate that the lock \
                        directory has been tampered with. Please avoid making manual \
                        changes to tool lock directories. The tool will now be relocked."
                       (Dune_pkg.Dev_tool.exe_name dev_tool)
                       (Package_name.to_string package_name)
                   ]))
        | Some pkg ->
          (match
             Dune_pkg.Dev_tool.needs_to_build_with_same_compiler_as_project dev_tool
           with
           | false -> Memo.return (`Lockdir_ok_with_tool_pkg pkg)
           | true ->
             let open Memo.O in
             let* compiler = compiler_package () in
             (match Package_name.Map.find packages compiler.info.name with
              | None -> Memo.return `No_compiler_lockfile_in_lockdir
              | Some pkg ->
                let+ ocaml_compiler = compiler_package () in
                (match Lock_dir.Pkg.equal pkg ocaml_compiler with
                 | true -> `Lockdir_ok_with_tool_pkg pkg
                 | false ->
                   `Dev_tool_needs_to_be_relocked_because_project_compiler_version_changed
                     (User_message.make
                        [ Pp.textf
                            "The version of the compiler package (%S) in this project's \
                             lockdir has changed to %s (formerly the compiler version \
                             was %s). The dev-tool %S will be re-locked and rebuilt with \
                             this version of the compiler."
                            (Package_name.to_string compiler.info.name)
                            (Package_version.to_string ocaml_compiler.info.version)
                            (Package_version.to_string pkg.info.version)
                            (Dune_pkg.Dev_tool.package_name dev_tool
                             |> Package_name.to_string)
                        ]))))))
;;

(* [lock_dev_tool_at_version dev_tool version] generates the lockdir for the
   dev tool [dev_tool]. If [version] is [Some v] then version [v] of the tool
   will be chosen by the solver. Otherwise the solver is free to choose the
   appropriate version of the tool to install. *)
let lock_dev_tool_at_version dev_tool version =
  let open Memo.O in
  let* need_to_solve =
    lockdir_status dev_tool
    >>| function
    | `Lockdir_ok_with_tool_pkg (pkg : Dune_pkg.Lock_dir.Pkg.t) ->
      (match version with
       | None -> false
       | Some version ->
         (* If this function was passed a specific version, and the dev
            tool's lockfile contains a different version from the specified
            version, regenerate the lockdir. *)
         let different_version_currently_locked =
           not (Package_version.equal pkg.info.version version)
         in
         if different_version_currently_locked
         then
           Console.print
             [ Pp.textf
                 "The lock directory for the tool %S exists but contains a solution for \
                  %s of the tool, whereas version %s now needs to be installed. The tool \
                  will now be re-locked."
                 (Dune_pkg.Dev_tool.exe_name dev_tool)
                 (Package_version.to_string pkg.info.version)
                 (Package_version.to_string version)
             ];
         different_version_currently_locked)
    | `No_lockdir -> true
    | `No_compiler_lockfile_in_lockdir ->
      Console.print
        [ Pp.textf
            "The lockdir for %s lacks a lockfile for %s. Regenerating..."
            (Dune_pkg.Dev_tool.package_name dev_tool |> Package_name.to_string)
            "ocaml"
        ];
      true
    | `Dev_tool_needs_to_be_relocked_because_project_compiler_version_changed message ->
      Console.print_user_message message;
      true
    | `Lockdir_missing_entry_for_tool message ->
      User_warning.emit_message message;
      true
  in
  if need_to_solve
  then
    let* extra_dependencies = extra_dependencies dev_tool in
    let local_pkg =
      make_local_package_wrapping_dev_tool
        ~dev_tool
        ~dev_tool_version:version
        ~extra_dependencies
    in
    let local_packages = Package_name.Map.singleton local_pkg.name local_pkg in
    solve ~dev_tool ~local_packages
  else Memo.return ()
;;

let lock_ocamlformat () =
  let version = Dune_pkg.Ocamlformat.version_of_current_project's_ocamlformat_config () in
  lock_dev_tool_at_version Ocamlformat version
;;

let lock_dev_tool dev_tool =
  match (dev_tool : Dune_pkg.Dev_tool.t) with
  | Ocamlformat -> lock_ocamlformat ()
  | other -> lock_dev_tool_at_version other None
;;
