open Import
open Pkg_common
module Package_version = Dune_pkg.Package_version
module Opam_repo = Dune_pkg.Opam_repo
module Repository_id = Dune_pkg.Repository_id
module Lock_dir = Dune_pkg.Lock_dir

let solve_lock_dir
  workspace
  ~local_packages
  version_preference
  ~update_opam_repositories
  solver_env_from_current_system
  lock_dir_path
  =
  let open Fiber.O in
  let lock_dir = Workspace.find_lock_dir workspace lock_dir_path in
  let solver_env =
    solver_env
      ~solver_env_from_context:
        (Option.bind lock_dir ~f:(fun lock_dir -> lock_dir.solver_env))
      ~solver_env_from_current_system
      ~unset_solver_vars_from_context:
        (unset_solver_vars_of_workspace workspace ~lock_dir_path)
  in
  let overlay =
    Console.Status_line.add_overlay (Constant (Pp.text "Solving for Build Plan"))
  in
  let* repos =
    repositories_of_workspace workspace
    |> get_repos
         ~repositories:(repositories_of_lock_dir workspace ~lock_dir_path)
         ~update_opam_repositories
  in
  Fiber.finalize
    ~finally:(fun () ->
      Console.Status_line.remove_overlay overlay;
      Fiber.return ())
    (fun () ->
      Dune_pkg.Opam_solver.solve_lock_dir
        solver_env
        (Pkg_common.Version_preference.choose
           ~from_arg:version_preference
           ~from_context:
             (Option.bind lock_dir ~f:(fun lock_dir -> lock_dir.version_preference)))
        repos
        ~local_packages:
          (Package_name.Map.map local_packages ~f:Dune_pkg.Local_package.for_solver)
        ~constraints:(constraints_of_workspace workspace ~lock_dir_path))
  >>= function
  | Error (`Diagnostic_message message) -> Fiber.return (Error (lock_dir_path, message))
  | Ok { lock_dir; files; _ } ->
    let summary_message =
      User_message.make
        [ Pp.tag
            User_message.Style.Success
            (Pp.textf
               "Solution for %s:"
               (Path.Source.to_string_maybe_quoted lock_dir_path))
        ; (match Package_name.Map.values lock_dir.packages with
           | [] ->
             Pp.tag User_message.Style.Warning @@ Pp.text "(no dependencies to lock)"
           | packages -> pp_packages packages)
        ]
    in
    let+ lock_dir = Lock_dir.compute_missing_checksums lock_dir in
    Ok (Lock_dir.Write_disk.prepare ~lock_dir_path ~files lock_dir, summary_message)
;;

let solve
  workspace
  ~update_opam_repositories
  ~solver_env_from_current_system
  ~version_preference
  ~lock_dirs_arg
  =
  let open Fiber.O in
  (* a list of thunks that will perform all the file IO side
     effects after performing validation so that if materializing any
     lockdir would fail then no side effect takes place. *)
  (let+ errors, solutions =
     let* local_packages = find_local_packages in
     Pkg_common.Lock_dirs_arg.lock_dirs_of_workspace lock_dirs_arg workspace
     |> Fiber.parallel_map
          ~f:
            (solve_lock_dir
               workspace
               ~local_packages
               version_preference
               ~update_opam_repositories
               solver_env_from_current_system)
     >>| List.partition_map ~f:Result.to_either
   in
   match errors with
   | [] -> Ok solutions
   | _ -> Error errors)
  >>| function
  | Error errors ->
    User_error.raise
      ([ Pp.text "Unable to solve dependencies for the following lock directories:" ]
       @ List.concat_map errors ~f:(fun (path, message) ->
         [ Pp.textf "Lock directory %s:" (Path.Source.to_string_maybe_quoted path)
         ; Pp.hovbox message
         ]))
  | Ok write_disks_with_summaries ->
    let write_disk_list, summary_messages = List.split write_disks_with_summaries in
    List.iter summary_messages ~f:Console.print_user_message;
    (* All the file IO side effects happen here: *)
    List.iter write_disk_list ~f:Lock_dir.Write_disk.commit
;;

let lock ~version_preference ~update_opam_repositories ~lock_dirs_arg =
  let open Fiber.O in
  let* workspace = Memo.run (Workspace.workspace ())
  and* solver_env_from_current_system =
    Dune_pkg.Sys_poll.solver_env_from_current_system
      ~path:(Env_path.path Stdune.Env.initial)
    >>| Option.some
  in
  solve
    workspace
    ~update_opam_repositories
    ~solver_env_from_current_system
    ~version_preference
    ~lock_dirs_arg
;;

let term =
  let+ builder = Common.Builder.term
  and+ version_preference = Version_preference.term
  and+ skip_update =
    Arg.(
      value
      & flag
      & info
          [ "skip-update" ]
          ~doc:
            "Do not fetch updates of opam repositories, will use the cached opam \
             metadata. This allows offline use if the repositories are cached locally.")
  and+ lock_dirs_arg = Pkg_common.Lock_dirs_arg.term in
  let builder = Common.Builder.forbid_builds builder in
  let common, config = Common.init builder in
  Scheduler.go ~common ~config (fun () ->
    lock ~version_preference ~update_opam_repositories:(not skip_update) ~lock_dirs_arg)
;;

let info =
  let doc = "Create a lockfile" in
  Cmd.info "lock" ~doc
;;

let command = Cmd.v info term
