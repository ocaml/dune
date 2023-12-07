open Import

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
  (let* local_packages = Pkg_common.find_local_packages in
   let+ solutions =
     Pkg_common.Lock_dirs_arg.lock_dirs_of_workspace lock_dirs_arg workspace
     |> Fiber.parallel_map ~f:(fun lock_dir_path ->
       let lock_dir = Workspace.find_lock_dir workspace lock_dir_path in
       let solver_env =
         Pkg_common.solver_env
           ~solver_env_from_context:
             (Option.bind lock_dir ~f:(fun lock_dir -> lock_dir.solver_env))
           ~solver_env_from_current_system
           ~unset_solver_vars_from_context:
             (Pkg_common.unset_solver_vars_of_workspace workspace ~lock_dir_path)
       in
       let* repos =
         Pkg_common.get_repos
           (Pkg_common.repositories_of_workspace workspace)
           ~repositories:(Pkg_common.repositories_of_lock_dir workspace ~lock_dir_path)
           ~update_opam_repositories
       in
       let overlay =
         Console.Status_line.add_overlay (Constant (Pp.text "Solving for Build Plan"))
       in
       Fiber.finalize
         ~finally:(fun () ->
           Console.Status_line.remove_overlay overlay;
           Fiber.return ())
         (fun () ->
           Opam_solver.solve_lock_dir
             solver_env
             (Pkg_common.Version_preference.choose
                ~from_arg:version_preference
                ~from_context:
                  (Option.bind lock_dir ~f:(fun lock_dir -> lock_dir.version_preference)))
             repos
             ~local_packages:
               (Package_name.Map.map local_packages ~f:Dune_pkg.Local_package.for_solver)
             ~constraints:(Pkg_common.constraints_of_workspace workspace ~lock_dir_path))
       >>= function
       | Error (`Diagnostic_message message) ->
         Fiber.return (Error (lock_dir_path, message))
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
                | packages -> Pkg_common.pp_packages packages)
             ]
         in
         let+ lock_dir = Lock_dir.compute_missing_checksums lock_dir in
         Ok (Lock_dir.Write_disk.prepare ~lock_dir_path ~files lock_dir, summary_message))
   in
   Result.List.all solutions)
  >>| function
  | Error (lock_dir_path, message) ->
    User_error.raise
      [ Pp.textf
          "Unable to solve dependencies for %s:"
          (Path.Source.to_string_maybe_quoted lock_dir_path)
      ; message
      ]
  | Ok write_disks_with_summaries ->
    let write_disk_list, summary_messages = List.split write_disks_with_summaries in
    List.iter summary_messages ~f:Console.print_user_message;
    (* All the file IO side effects happen here: *)
    List.iter write_disk_list ~f:Lock_dir.Write_disk.commit
;;

let lock
  ~dont_poll_system_solver_variables
  ~version_preference
  ~update_opam_repositories
  ~lock_dirs_arg
  =
  let open Fiber.O in
  let* workspace = Memo.run (Workspace.workspace ())
  and* solver_env_from_current_system =
    if dont_poll_system_solver_variables
    then Fiber.return None
    else
      Sys_poll.solver_env_from_current_system ~path:(Env_path.path Stdune.Env.initial)
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
  and+ version_preference = Pkg_common.Version_preference.term
  and+ dont_poll_system_solver_variables =
    Arg.(
      value
      & flag
      & info
          [ "dont-poll-system-solver-variables" ]
          ~doc:
            "Don't derive system solver variables from the current system. Values \
             assigned to these variables in build contexts will still be used. Note that \
             Opam filters that depend on unset variables resolve to the value \
             \"undefined\" which is treated as false. For example if a dependency has a \
             filter `{os = \"linux\"}` and the variable \"os\" is unset, the dependency \
             will be excluded. ")
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
    lock
      ~dont_poll_system_solver_variables
      ~version_preference
      ~update_opam_repositories:(not skip_update)
      ~lock_dirs_arg)
;;

let info =
  let doc = "Create a lockfile" in
  Cmd.info "lock" ~doc
;;

let command = Cmd.v info term
