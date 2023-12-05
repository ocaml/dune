open Import
open Pkg_common

let print_solver_env_for_lock_dir workspace ~solver_env_from_current_system lock_dir_path =
  let solver_env_from_context =
    Option.bind (Workspace.find_lock_dir workspace lock_dir_path) ~f:(fun lock_dir ->
      lock_dir.solver_env)
  in
  let solver_env =
    solver_env
      ~solver_env_from_current_system
      ~solver_env_from_context
      ~unset_solver_vars_from_context:
        (Pkg_common.unset_solver_vars_of_workspace workspace ~lock_dir_path)
  in
  Console.print
    [ Pp.textf
        "Solver environment for lock directory %s:"
        (Path.Source.to_string_maybe_quoted lock_dir_path)
    ; Dune_pkg.Solver_env.pp solver_env
    ]
;;

let print_solver_env ~dont_poll_system_solver_variables =
  let open Fiber.O in
  let+ workspace = Memo.run (Workspace.workspace ())
  and+ solver_env_from_current_system =
    if dont_poll_system_solver_variables
    then Fiber.return None
    else
      Dune_pkg.Sys_poll.solver_env_from_current_system
        ~path:(Env_path.path Stdune.Env.initial)
      >>| Option.some
  in
  let lock_dirs = lock_dirs_of_workspace workspace in
  List.iter
    lock_dirs
    ~f:(print_solver_env_for_lock_dir workspace ~solver_env_from_current_system)
;;

let term =
  let+ builder = Common.Builder.term
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
  in
  let builder = Common.Builder.forbid_builds builder in
  let common, config = Common.init builder in
  Scheduler.go ~common ~config (fun () ->
    print_solver_env ~dont_poll_system_solver_variables)
;;

let info =
  let doc =
    "Print a description of the environment that would be used to solve dependencies and \
     then exit without attempting to solve the dependencies or generate the lockfile. \
     Intended to be used to debug situations where no solution can be found to a \
     project's dependencies."
  in
  Cmd.info "print-solver-env" ~doc
;;

let command = Cmd.v info term
