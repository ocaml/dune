open Dune_config
open Import
open Pkg_common
module Package_version = Dune_pkg.Package_version
module Opam_repo = Dune_pkg.Opam_repo
module Lock_dir = Dune_pkg.Lock_dir
module Pin_stanza = Dune_lang.Pin_stanza
module Pin = Dune_pkg.Pin
module Solver_env = Dune_pkg.Solver_env

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
      { lockdir_path : Path.t
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
          [ Pp.textf "Locking %s: " (Path.to_string_maybe_quoted lockdir_path)
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

module Platforms_by_message = struct
  module Message = struct
    type t =
      | Solve_error of User_message.Style.t Pp.t
      | Manifest_error of User_message.t

    let to_dyn = function
      | Solve_error message ->
        Dyn.variant "Solve_error" [ Pp.to_dyn User_message.Style.to_dyn message ]
      | Manifest_error message ->
        Dyn.variant "Manifest_error" [ User_message.to_dyn message ]
    ;;

    let compare a b =
      match a, b with
      | Solve_error a, Solve_error b -> Pp.compare ~compare:User_message.Style.compare a b
      | Solve_error _, _ -> Lt
      | _, Solve_error _ -> Gt
      | Manifest_error a, Manifest_error b -> User_message.compare a b
    ;;
  end

  module Message_map = Map.Make (Message)

  (* Map messages to the list of platforms for which those messages are
     relevant. If a dependency problem has no solution on any platform, it's
     likely that the error from the solver will be identical across all
     platforms. We don't want to print the same error message once for each
     platform, so this type collects messages and the platforms for which they
     are relevant, deduplicating common messages. *)
  type t = Solver_env.t list Message_map.t

  let singleton message platform : t = Message_map.singleton message [ platform ]
  let to_list (t : t) : (Message.t * Solver_env.t list) list = Message_map.to_list t
  let union_all ts : t = Message_map.union_all ts ~f:(fun _ a b -> Some (a @ b))

  let all_solver_errors_raising_if_any_manifest_errors t =
    let solver_errors, manifest_errors =
      List.partition_map (to_list t) ~f:(fun (message, platforms) ->
        match message with
        | Solve_error message -> Left (message, platforms)
        | Manifest_error message -> Right message)
    in
    match manifest_errors with
    | [] -> solver_errors
    | message :: _ -> raise (User_error.E message)
  ;;
end

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
    Solver_env.unset_multi
      base_solver_env
      Dune_lang.Package_variable_name.platform_specific
  in
  let+ results =
    Fiber.parallel_map solve_for_platforms ~f:(fun platform_env ->
      let solver_env = Solver_env.extend portable_solver_env platform_env in
      let+ solver_result = solve_for_env solver_env in
      Result.map_error solver_result ~f:(fun message ->
        let message : Platforms_by_message.Message.t =
          match message with
          | `Solve_error m -> Solve_error m
          | `Manifest_error m -> Manifest_error m
        in
        Platforms_by_message.singleton message platform_env))
  in
  let solver_results, errors =
    List.partition_map results ~f:(function
      | Ok result -> Left result
      | Error e -> Right e)
  in
  match solver_results, errors with
  | [], [] -> Code_error.raise "Solver did not run for any platforms." []
  | [], errors ->
    `All_error
      (Platforms_by_message.union_all errors
       |> Platforms_by_message.all_solver_errors_raising_if_any_manifest_errors)
  | x :: xs, errors ->
    let merged_solver_result =
      List.fold_left xs ~init:x ~f:Dune_pkg.Opam_solver.Solver_result.merge
    in
    if List.is_empty errors
    then `All_ok merged_solver_result
    else
      `Partial
        ( merged_solver_result
        , Platforms_by_message.union_all errors
          |> Platforms_by_message.all_solver_errors_raising_if_any_manifest_errors )
;;

let summary_message
      ~portable_lock_dir
      ~lock_dir_path
      ~(lock_dir : Lock_dir.t)
      ~maybe_perf_stats
      ~maybe_unsolved_platforms_message
  =
  if portable_lock_dir
  then (
    let pkgs_by_platform = Lock_dir.Packages.pkgs_by_platform lock_dir.packages in
    let opam_package_of_pkg (pkg : Lock_dir.Pkg.t) =
      OpamPackage.create
        (Dune_pkg.Package_name.to_opam_package_name pkg.info.name)
        (Dune_pkg.Package_version.to_opam_package_version pkg.info.version)
    in
    let pkgs_by_opam_package =
      Lock_dir.Packages.to_pkg_list lock_dir.packages
      |> List.map ~f:(fun pkg -> opam_package_of_pkg pkg, pkg)
      |> OpamPackage.Map.of_list
    in
    let opam_package_to_pkg opam_package =
      OpamPackage.Map.find opam_package pkgs_by_opam_package
    in
    let opam_package_sets_by_platform =
      Solver_env.Map.map pkgs_by_platform ~f:(fun pkgs ->
        List.map pkgs ~f:opam_package_of_pkg |> OpamPackage.Set.of_list)
    in
    let common_packages =
      Solver_env.Map.values opam_package_sets_by_platform
      |> List.reduce ~f:OpamPackage.Set.inter
      |> Option.value ~default:OpamPackage.Set.empty
    in
    let pp_package_set package_set =
      if OpamPackage.Set.is_empty package_set
      then Pp.tag User_message.Style.Warning @@ Pp.text "(none)"
      else (
        let pkgs =
          OpamPackage.Set.elements package_set |> List.map ~f:opam_package_to_pkg
        in
        Pkg_common.pp_packages pkgs)
    in
    let uncommon_packages_by_platform =
      Solver_env.Map.map opam_package_sets_by_platform ~f:(fun package_set ->
        OpamPackage.Set.diff package_set common_packages)
      |> Solver_env.Map.filteri ~f:(fun _ package_set ->
        not (OpamPackage.Set.is_empty package_set))
    in
    let maybe_uncommon_packages =
      if Solver_env.Map.is_empty uncommon_packages_by_platform
      then []
      else
        Pp.nop
        :: Pp.text "Additionally, some packages will only be built on specific platforms."
        :: (Solver_env.Map.to_list uncommon_packages_by_platform
            |> List.concat_map ~f:(fun (platform, packages) ->
              [ Pp.nop
              ; Pp.concat [ Solver_env.pp_oneline platform; Pp.text ":" ]
              ; pp_package_set packages
              ]))
    in
    (Pp.tag
       User_message.Style.Success
       (Pp.textf "Solution for %s" (Path.to_string_maybe_quoted lock_dir_path))
     :: Pp.nop
     :: Pp.text "Dependencies common to all supported platforms:"
     :: pp_package_set common_packages
     :: (maybe_uncommon_packages @ maybe_perf_stats))
    @ maybe_unsolved_platforms_message)
  else
    (Pp.tag
       User_message.Style.Success
       (Pp.textf "Solution for %s:" (Path.to_string_maybe_quoted lock_dir_path))
     :: (match Lock_dir.Packages.to_pkg_list lock_dir.packages with
         | [] -> Pp.tag User_message.Style.Warning @@ Pp.text "(no dependencies to lock)"
         | packages -> pp_packages packages)
     :: maybe_perf_stats)
    @ maybe_unsolved_platforms_message
;;

let pp_solve_errors_by_platforms platforms_by_message =
  List.map platforms_by_message ~f:(fun (message, platforms) ->
    Pp.concat
      ~sep:Pp.cut
      [ Pp.nop
      ; Pp.box
        @@ Pp.text
             "The dependency solver failed to find a solution for the following \
              platforms:"
      ; Pp.enumerate platforms ~f:Solver_env.pp_oneline
      ; Pp.box @@ Pp.text "...with this error:"
      ; message
      ]
    |> Pp.vbox)
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
    | None -> project_pins, Solver_env.popular_platform_envs
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
    solver_env
      ~solver_env_from_context
      ~solver_env_from_current_system
      ~unset_solver_vars_from_context:
        (unset_solver_vars_of_workspace workspace ~lock_dir_path)
  in
  let solve_for_platforms =
    match portable_lock_dir with
    | true ->
      (match solver_env_from_context with
       | Some solver_env_from_context ->
         List.map solve_for_platforms ~f:(fun platform_env ->
           Solver_env.extend solver_env_from_context platform_env)
       | None -> solve_for_platforms)
    | false -> [ solver_env ]
  in
  let time_start = Unix.gettimeofday () in
  let* repos =
    let repo_map = repositories_of_workspace workspace in
    let repo_names =
      Dune_pkg.Pkg_workspace.Repository.Name.Map.keys repo_map
      |> List.sort ~compare:Dune_pkg.Pkg_workspace.Repository.Name.compare
    in
    progress_state
    := Some (Progress_indicator.Per_lockdir.State.Updating_repos repo_names);
    get_repos repo_map ~repositories:(repositories_of_lock_dir workspace ~lock_dir_path)
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
      ~constraints:(constraints_of_workspace workspace ~lock_dir_path)
      ~selected_depopts:(depopts_of_workspace workspace ~lock_dir_path)
      ~solve_for_platforms
      ~portable_lock_dir
  in
  let solver_result =
    match result with
    | `All_error messages -> Error messages
    | `All_ok solver_result -> Ok (solver_result, [])
    | `Partial (solver_result, errors) ->
      Log.info @@ pp_solve_errors_by_platforms errors;
      let all_platforms =
        List.concat_map errors ~f:snd |> List.sort_uniq ~compare:Solver_env.compare
      in
      Ok
        ( solver_result
        , [ Pp.nop
          ; Pp.tag User_message.Style.Warning
            @@ Pp.vbox
            @@ Pp.concat
                 ~sep:Pp.cut
                 [ Pp.box
                   @@ Pp.text "No package solution was found for some requsted platforms."
                 ; Pp.nop
                 ; Pp.box @@ Pp.text "Platforms with no solution:"
                 ; Pp.box @@ Pp.enumerate all_platforms ~f:Solver_env.pp_oneline
                 ; Pp.nop
                 ; Pp.box
                   @@ Pp.text
                        "See the log or run with --verbose for more details. Configure \
                         platforms to solve for in the dune-workspace file."
                 ]
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
        (summary_message
           ~portable_lock_dir
           ~lock_dir_path
           ~lock_dir
           ~maybe_perf_stats
           ~maybe_unsolved_platforms_message)
    in
    progress_state := None;
    let+ lock_dir = Lock_dir.compute_missing_checksums ~pinned_packages lock_dir in
    Ok
      ( Lock_dir.Write_disk.prepare ~portable_lock_dir ~lock_dir_path ~files lock_dir
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
    if portable_lock_dir
    then
      User_error.raise
        (List.concat_map errors ~f:(fun (path, errors) ->
           [ Pp.box
             @@ Pp.textf
                  "Unable to solve dependencies while generating lock directory: %s"
                  (Path.to_string_maybe_quoted path)
           ; Pp.vbox (Pp.concat ~sep:Pp.cut (pp_solve_errors_by_platforms errors))
           ]))
    else
      User_error.raise
        ([ Pp.text "Unable to solve dependencies for the following lock directories:" ]
         @ List.concat_map errors ~f:(fun (path, errors) ->
           let messages = List.map errors ~f:fst in
           [ Pp.textf "Lock directory %s:" (Path.to_string_maybe_quoted path)
           ; Pp.vbox (Pp.concat ~sep:Pp.cut messages)
           ]))
  | Ok write_disks_with_summaries ->
    let write_disk_list, summary_messages = List.split write_disks_with_summaries in
    List.iter summary_messages ~f:Console.print_user_message;
    (* All the file IO side effects happen here: *)
    List.iter write_disk_list ~f:Lock_dir.Write_disk.commit
;;

let project_pins =
  let open Memo.O in
  Dune_rules.Dune_load.projects ()
  >>| List.fold_left ~init:Pin.DB.empty ~f:(fun acc project ->
    let pins = project_and_package_pins project in
    Pin.DB.combine_exn acc pins)
;;

let lock ~version_preference ~lock_dirs_arg ~print_perf_stats ~portable_lock_dir =
  let open Fiber.O in
  let* solver_env_from_current_system =
    poll_solver_env_from_current_system () >>| Option.some
  and* workspace, local_packages, project_pins =
    Memo.run
    @@
    let open Memo.O in
    let+ workspace = Workspace.workspace ()
    and+ local_packages = find_local_packages
    and+ project_pins = project_pins in
    workspace, local_packages, project_pins
  in
  let lock_dirs =
    Pkg_common.Lock_dirs_arg.lock_dirs_of_workspace lock_dirs_arg workspace
    |> List.map ~f:Path.source
  in
  solve
    workspace
    ~local_packages
    ~project_pins
    ~solver_env_from_current_system
    ~version_preference
    ~lock_dirs
    ~print_perf_stats
    ~portable_lock_dir
;;

let term =
  let+ builder = Common.Builder.term
  and+ version_preference = Version_preference.term
  and+ lock_dirs_arg = Pkg_common.Lock_dirs_arg.term
  and+ print_perf_stats = Arg.(value & flag & info [ "print-perf-stats" ]) in
  let builder = Common.Builder.forbid_builds builder in
  let common, config = Common.init builder in
  Scheduler.go_with_rpc_server ~common ~config (fun () ->
    let open Fiber.O in
    Pkg_common.check_pkg_management_enabled ()
    >>>
    let portable_lock_dir =
      match Config.get Dune_rules.Compile_time.portable_lock_dir with
      | `Enabled -> true
      | `Disabled -> false
    in
    lock ~version_preference ~lock_dirs_arg ~print_perf_stats ~portable_lock_dir)
;;

let info =
  let doc = "Create a lockfile" in
  Cmd.info "lock" ~doc
;;

let command = Cmd.v info term
