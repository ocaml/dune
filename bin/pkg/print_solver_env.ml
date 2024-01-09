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

let print_solver_env ~lock_dirs_arg =
  let open Fiber.O in
  let+ workspace = Memo.run (Workspace.workspace ())
  and+ solver_env_from_current_system =
    Dune_pkg.Sys_poll.make ~path:(Env_path.path Stdune.Env.initial)
    |> Dune_pkg.Sys_poll.solver_env_from_current_system
    >>| Option.some
  in
  let lock_dirs = Lock_dirs_arg.lock_dirs_of_workspace lock_dirs_arg workspace in
  List.iter
    lock_dirs
    ~f:(print_solver_env_for_lock_dir workspace ~solver_env_from_current_system)
;;

let term =
  let+ builder = Common.Builder.term
  and+ lock_dirs_arg = Lock_dirs_arg.term in
  let builder = Common.Builder.forbid_builds builder in
  let common, config = Common.init builder in
  Scheduler.go ~common ~config (fun () -> print_solver_env ~lock_dirs_arg)
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
