open Dune_config
open Import
module Lock_dir = Dune_pkg.Lock_dir
module Pin = Dune_pkg.Pin

module Progress_indicator = struct
  module Per_lockdir = struct
    module State = struct
      module Repository = Dune_pkg.Pkg_workspace.Repository

      type t =
        | Updating_repos of Repository.Name.t list
        | Solving

      let pp = function
        | Updating_repos repo_names ->
          Pp.textf
            "Updating package repos %s..."
            (List.map repo_names ~f:(fun repo_name ->
               Repository.Name.to_string repo_name |> String.quoted)
             |> String.enumerate_and)
        | Solving -> Pp.text "Solving..."
      ;;
    end

    type t =
      { lockdir_path : Path.Build.t
      ; state : State.t option ref
      }

    let create lockdir_path = { lockdir_path; state = ref None }
  end

  (* The progress indicator for the entire lock operation, which may
     involve generating multiple lockdirs *)
  type t = Per_lockdir.t list

  let pp (t : t) =
    (* Only display the first non-done lockdir state, since the status
       line can only consist of a single line. *)
    List.find_map t ~f:(fun { Per_lockdir.lockdir_path; state } ->
      Option.map !state ~f:(fun state ->
        Pp.concat
          [ Pp.textf "Locking %s: " (Path.Build.to_string_maybe_quoted lockdir_path)
          ; Per_lockdir.State.pp state
          ]))
    |> Option.value ~default:Pp.nop
  ;;

  let add_overlay (t : t) = Console.Status_line.add_overlay (Live (fun () -> pp t))
end

let project_and_package_pins project =
  let dir = Dune_project.root project in
  let pins = Dune_project.pins project in
  let packages = Dune_project.packages project in
  Pin.DB.add_opam_pins (Pin.DB.of_stanza ~dir pins) packages
;;

(* For recursive pins, we must traverse the pinned sources. The [project_pins]
   are the initial pins that we have in our project. *)
let resolve_project_pins project_pins =
  let scan_project ~read ~files =
    let read file = Memo.of_reproducible_fiber (read file) in
    let open Memo.O in
    (* Opam files may never contain recursive pins, so don't both reading them *)
    Dune_project.gen_load
      ~read
      ~files
      ~dir:Path.Source.root
      ~infer_from_opam_files:false
      ~load_opam_file_with_contents:Dune_pkg.Opam_file.load_opam_file_with_contents
    >>| Option.map ~f:(fun project ->
      let packages = Dune_project.packages project in
      let pins = project_and_package_pins project in
      pins, packages)
    |> Memo.run
  in
  Pin.resolve project_pins ~scan_project
;;

let solve_multiple_platforms
      base_solver_env
      version_preference
      repos
      ~pins
      ~local_packages
      ~constraints
      ~selected_depopts
      ~solve_for_platforms
      ~portable_lock_dir
  =
  let open Fiber.O in
  let solve_for_env env =
    Dune_pkg.Opam_solver.solve_lock_dir
      env
      version_preference
      repos
      ~pins
      ~local_packages
      ~constraints
      ~selected_depopts
      ~portable_lock_dir
  in
  let portable_solver_env =
    Dune_pkg.Solver_env.unset_multi
      base_solver_env
      Dune_lang.Package_variable_name.platform_specific
  in
  let+ results =
    Fiber.parallel_map solve_for_platforms ~f:(fun platform_env ->
      let solver_env = Dune_pkg.Solver_env.extend portable_solver_env platform_env in
      solve_for_env solver_env)
  in
  let solver_results, errors =
    List.partition_map results ~f:(function
      | Ok result -> Left result
      | Error (`Diagnostic_message message) -> Right message)
  in
  match solver_results, errors with
  | [], [] -> Code_error.raise "Solver did not run for any platforms." []
  | [], errors -> `All_error errors
  | x :: xs, errors ->
    let merged_solver_result =
      List.fold_left xs ~init:x ~f:Dune_pkg.Opam_solver.Solver_result.merge
    in
    if List.is_empty errors
    then `All_ok merged_solver_result
    else `Partial (merged_solver_result, errors)
;;

let solve_lock_dir
      workspace
      ~local_packages
      ~project_pins
      ~print_perf_stats
      ~portable_lock_dir
      version_preference
      solver_env_from_current_system
      lock_dir_path
      progress_state
  =
  let open Fiber.O in
  let lock_dir = Workspace.find_lock_dir workspace lock_dir_path in
  let project_pins, solve_for_platforms =
    match lock_dir with
    | None -> project_pins, Dune_pkg.Solver_env.popular_platform_envs
    | Some lock_dir ->
      let workspace =
        Pin.DB.Workspace.of_stanza workspace.pins
        |> Pin.DB.Workspace.extract ~names:lock_dir.pins
      in
      Pin.DB.combine_exn workspace project_pins, lock_dir.solve_for_platforms
  in
  let solver_env_from_context =
    Option.bind lock_dir ~f:(fun lock_dir -> lock_dir.solver_env)
  in
  let solver_env =
    Pkg_common.solver_env
      ~solver_env_from_context
      ~solver_env_from_current_system
      ~unset_solver_vars_from_context:
        (Pkg_common.unset_solver_vars_of_workspace workspace ~lock_dir_path)
  in
  let solve_for_platforms =
    match portable_lock_dir with
    | true ->
      (match solver_env_from_context with
       | Some solver_env_from_context ->
         List.map solve_for_platforms ~f:(fun platform_env ->
           Dune_pkg.Solver_env.extend solver_env_from_context platform_env)
       | None -> solve_for_platforms)
    | false -> [ solver_env ]
  in
  let time_start = Unix.gettimeofday () in
  let* repos =
    let repo_map = Pkg_common.repositories_of_workspace workspace in
    let repo_names =
      Dune_pkg.Pkg_workspace.Repository.Name.Map.keys repo_map
      |> List.sort ~compare:Dune_pkg.Pkg_workspace.Repository.Name.compare
    in
    progress_state
    := Some (Progress_indicator.Per_lockdir.State.Updating_repos repo_names);
    Pkg_common.get_repos
      repo_map
      ~repositories:(Pkg_common.repositories_of_lock_dir workspace ~lock_dir_path)
  in
  let* pins = resolve_project_pins project_pins in
  let time_solve_start = Unix.gettimeofday () in
  progress_state := Some Progress_indicator.Per_lockdir.State.Solving;
  let* result =
    solve_multiple_platforms
      solver_env
      (Pkg_common.Version_preference.choose
         ~from_arg:version_preference
         ~from_context:
           (Option.bind lock_dir ~f:(fun lock_dir -> lock_dir.version_preference)))
      repos
      ~pins
      ~local_packages:
        (Package_name.Map.map local_packages ~f:Dune_pkg.Local_package.for_solver)
      ~constraints:(Pkg_common.constraints_of_workspace workspace ~lock_dir_path)
      ~selected_depopts:(Pkg_common.depopts_of_workspace workspace ~lock_dir_path)
      ~solve_for_platforms
      ~portable_lock_dir
  in
  let solver_result =
    match result with
    | `All_error messages -> Error messages
    | `All_ok solver_result -> Ok (solver_result, [])
    | `Partial (solver_result, errors) ->
      Log.info errors;
      Ok
        ( solver_result
        , [ Pp.nop
          ; Pp.text
              "No solution was found for some platforms. See the log or run with \
               --verbose for more details."
            |> Pp.tag User_message.Style.Warning
          ] )
  in
  match solver_result with
  | Error messages -> Fiber.return (Error (lock_dir_path, messages))
  | Ok (solver_result, maybe_unsolved_platforms_message) ->
    let { Dune_pkg.Opam_solver.Solver_result.lock_dir
        ; files
        ; pinned_packages
        ; num_expanded_packages
        }
      =
      solver_result
    in
    let time_end = Unix.gettimeofday () in
    let maybe_perf_stats =
      if print_perf_stats
      then
        [ Pp.nop
        ; Pp.textf "Expanded packages: %d" num_expanded_packages
        ; Pp.textf "Updated repos in: %.2fs" (time_solve_start -. time_start)
        ; Pp.textf "Solved dependencies in: %.2fs" (time_end -. time_solve_start)
        ]
      else []
    in
    let summary_message =
      User_message.make
        ((Pp.tag
            User_message.Style.Success
            (Pp.textf
               "Solution for %s:"
               (lock_dir_path
                |> Path.Build.drop_build_context_exn
                |> Path.Source.to_string_maybe_quoted))
          :: (match Lock_dir.Packages.to_pkg_list lock_dir.packages with
              | [] ->
                Pp.tag User_message.Style.Warning @@ Pp.text "(no dependencies to lock)"
              | packages -> Pkg_common.pp_packages packages)
          :: maybe_perf_stats)
         @ maybe_unsolved_platforms_message)
    in
    progress_state := None;
    let+ lock_dir = Lock_dir.compute_missing_checksums ~pinned_packages lock_dir in
    Ok
      ( Lock_dir.Write_disk.prepare
          ~portable_lock_dir
          ~lock_dir_path:(Path.build lock_dir_path)
          ~files
          lock_dir
      , summary_message )
;;

let solve
      workspace
      ~local_packages
      ~project_pins
      ~solver_env_from_current_system
      ~version_preference
      ~lock_dirs
      ~print_perf_stats
      ~portable_lock_dir
  =
  let open Fiber.O in
  (* a list of thunks that will perform all the file IO side
     effects after performing validation so that if materializing any
     lockdir would fail then no side effect takes place. *)
  (let+ errors, solutions =
     let progress_indicator =
       List.map lock_dirs ~f:Progress_indicator.Per_lockdir.create
     in
     let overlay = Progress_indicator.add_overlay progress_indicator in
     let+ result =
       Fiber.finalize
         ~finally:(fun () ->
           Console.Status_line.remove_overlay overlay;
           Fiber.return ())
         (fun () ->
            Fiber.parallel_map progress_indicator ~f:(fun { lockdir_path; state } ->
              solve_lock_dir
                workspace
                ~local_packages
                ~project_pins
                ~print_perf_stats
                ~portable_lock_dir
                version_preference
                solver_env_from_current_system
                lockdir_path
                state))
     in
     List.partition_map result ~f:Result.to_either
   in
   match errors with
   | [] -> Ok solutions
   | _ -> Error errors)
  >>| function
  | Error errors ->
    User_error.raise
      ([ Pp.text "Unable to solve dependencies for the following lock directories:" ]
       @ List.concat_map errors ~f:(fun (path, messages) ->
         [ Pp.textf "Lock directory %s:" (Path.Build.to_string_maybe_quoted path)
         ; Pp.hovbox (Pp.concat ~sep:Pp.newline messages)
         ]))
  | Ok write_disks_with_summaries ->
    let write_disk_list, summary_messages = List.split write_disks_with_summaries in
    List.iter summary_messages ~f:Console.print_user_message;
    (* All the file IO side effects happen here: *)
    List.iter write_disk_list ~f:Lock_dir.Write_disk.commit
;;

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

let solve ctx_name ~dev_tool ~local_packages =
  let open Memo.O in
  let* solver_env_from_current_system =
    Pkg_common.poll_solver_env_from_current_system ()
    |> Memo.of_reproducible_fiber
    >>| Option.some
  and* workspace =
    let+ workspace = Workspace.workspace () in
    match Config.get Dune_rules.Compile_time.bin_dev_tools with
    | `Enabled ->
      Workspace.add_repo workspace Dune_pkg.Pkg_workspace.Repository.binary_packages
    | `Disabled -> workspace
  in
  let lock_dir = Lock_dir.dev_tool_lock_dir_path ctx_name dev_tool in
  Memo.of_reproducible_fiber
  @@ solve
       workspace
       ~local_packages
       ~project_pins:Pin.DB.empty
       ~solver_env_from_current_system
       ~version_preference:None
       ~lock_dirs:[ lock_dir ]
       ~print_perf_stats:false
       ~portable_lock_dir:false
;;

let compiler_package_name = Package_name.of_string "ocaml"

(* Some dev tools must be built with the same version of the ocaml
   compiler as the project. This function returns the version of the
   "ocaml" package used to compile the project in the default build
   context.

   TODO: This only makes sure that the version of compiler used to
   build the dev tool matches the version of the compiler used to
   build this project. This will fail if the project is built with a
   custom compiler (e.g. ocaml-variants) since the version of the
   compiler will be the same between the project and dev tool while
   they still use different compilers. A more robust solution would be
   to ensure that the exact compiler package used to build the dev
   tool matches the package used to build the compiler. *)
let locked_ocaml_compiler_version () =
  let open Memo.O in
  let context =
    (* Dev tools are only ever built with the default context. *)
    Context_name.default
  in
  let* result = Dune_rules.Lock_dir.get context
  and* platform =
    Pkg_common.poll_solver_env_from_current_system () |> Memo.of_reproducible_fiber
  in
  match result with
  | Error _ ->
    User_error.raise
      [ Pp.text "Unable to load the lockdir for the default build context." ]
      ~hints:
        [ Pp.concat
            ~sep:Pp.space
            [ Pp.text "Try running"; User_message.command "dune pkg lock" ]
        ]
  | Ok { packages; _ } ->
    let packages = Lock_dir.Packages.pkgs_on_platform_by_name packages ~platform in
    (match Package_name.Map.find packages compiler_package_name with
     | None ->
       User_error.raise
         [ Pp.textf
             "The lockdir doesn't contain a lockfile for the package %S."
             (Package_name.to_string compiler_package_name)
         ]
         ~hints:
           [ Pp.concat
               ~sep:Pp.space
               [ Pp.textf
                   "Add a dependency on %S to one of the packages in dune-project and \
                    then run"
                   (Package_name.to_string compiler_package_name)
               ; User_message.command "dune pkg lock"
               ]
           ]
     | Some pkg -> Memo.return pkg.info.version)
;;

(* Returns a dependency constraint on the version of the ocaml
   compiler in the lockdir associated with the default context. *)
let locked_ocaml_compiler_constraint () =
  let open Dune_lang in
  let open Memo.O in
  let+ ocaml_compiler_version = locked_ocaml_compiler_version () in
  let constraint_ =
    Some
      (Package_constraint.Uop
         (Eq, String_literal (Package_version.to_string ocaml_compiler_version)))
  in
  { Package_dependency.name = compiler_package_name; constraint_ }
;;

let extra_dependencies dev_tool =
  let open Memo.O in
  match Dune_pkg.Dev_tool.needs_to_build_with_same_compiler_as_project dev_tool with
  | false -> Memo.return []
  | true ->
    let+ constraint_ = locked_ocaml_compiler_constraint () in
    [ constraint_ ]
;;

let lockdir_status ctx_name dev_tool =
  let open Memo.O in
  let dev_tool_lock_dir = Lock_dir.dev_tool_lock_dir_path ctx_name dev_tool in
  match Lock_dir.read_disk dev_tool_lock_dir with
  | Error _ -> Memo.return `No_lockdir
  | Ok { packages; _ } ->
    (match Dune_pkg.Dev_tool.needs_to_build_with_same_compiler_as_project dev_tool with
     | false -> Memo.return `Lockdir_ok
     | true ->
       let* platform =
         Pkg_common.poll_solver_env_from_current_system () |> Memo.of_reproducible_fiber
       in
       let packages = Lock_dir.Packages.pkgs_on_platform_by_name packages ~platform in
       (match Package_name.Map.find packages compiler_package_name with
        | None -> Memo.return `No_compiler_lockfile_in_lockdir
        | Some { info; _ } ->
          let+ ocaml_compiler_version = locked_ocaml_compiler_version () in
          (match Package_version.equal info.version ocaml_compiler_version with
           | true -> `Lockdir_ok
           | false ->
             `Dev_tool_needs_to_be_relocked_because_project_compiler_version_changed
               (User_message.make
                  [ Pp.textf
                      "The version of the compiler package (%S) in this project's \
                       lockdir has changed to %s (formerly the compiler version was %s). \
                       The dev-tool %S will be re-locked and rebuilt with this version \
                       of the compiler."
                      (Package_name.to_string compiler_package_name)
                      (Package_version.to_string ocaml_compiler_version)
                      (Package_version.to_string info.version)
                      (Dune_pkg.Dev_tool.package_name dev_tool |> Package_name.to_string)
                  ]))))
;;

(* [lock_dev_tool_at_version ctx_name dev_tool version] generates the lockdir
   for the dev tool [dev_tool]. If [version] is [Some v] then version [v] of
   the tool will be chosen by the solver. Otherwise the solver is free to
   choose the appropriate version of the tool to install. *)
let lock_dev_tool_at_version ctx_name dev_tool version =
  let open Memo.O in
  let* need_to_solve =
    lockdir_status ctx_name dev_tool
    >>| function
    | `Lockdir_ok -> false
    | `No_lockdir -> true
    | `No_compiler_lockfile_in_lockdir ->
      Console.print
        [ Pp.textf
            "The lockdir for %s lacks a lockfile for %s. Regenerating..."
            (Dune_pkg.Dev_tool.package_name dev_tool |> Package_name.to_string)
            (Package_name.to_string compiler_package_name)
        ];
      true
    | `Dev_tool_needs_to_be_relocked_because_project_compiler_version_changed message ->
      Console.print_user_message message;
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
    solve ctx_name ~dev_tool ~local_packages
  else Memo.return ()
;;

let lock_ocamlformat ctx_name () =
  let version = Dune_pkg.Ocamlformat.version_of_current_project's_ocamlformat_config () in
  lock_dev_tool_at_version ctx_name Ocamlformat version
;;

let lock_dev_tool ctx_name dev_tool =
  match (dev_tool : Dune_pkg.Dev_tool.t) with
  | Ocamlformat -> lock_ocamlformat ctx_name ()
  | other -> lock_dev_tool_at_version ctx_name other None
;;
