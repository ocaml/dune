open Import
open Pkg_common

let print_solver_env_for_one_context
  ~sys_bindings_from_current_system
  { Per_context.solver_sys_vars = solver_sys_vars_from_context
  ; context_common = { name = context_name; _ }
  ; _
  }
  =
  let solver_env =
    Dune_pkg.Solver_env.create
      ~sys:
        (solver_env_variables
           ~solver_sys_vars_from_context
           ~sys_bindings_from_current_system)
  in
  Console.print
    [ Pp.textf
        "Solver environment for context %s:"
        (String.maybe_quoted @@ Dune_engine.Context_name.to_string context_name)
    ; Dune_pkg.Solver_env.pp solver_env
    ]
;;

let print_solver_env
  ~context_name
  ~all_contexts
  ~version_preference
  ~dont_poll_system_solver_variables
  =
  let open Fiber.O in
  let+ per_context =
    Per_context.choose
      ~context_name_arg:context_name
      ~all_contexts_arg:all_contexts
      ~version_preference_arg:version_preference
  and+ sys_bindings_from_current_system =
    if dont_poll_system_solver_variables
    then Fiber.return Dune_pkg.Solver_env.Variable.Sys.Bindings.empty
    else Dune_pkg.Sys_poll.sys_bindings ~path:(Env_path.path Stdune.Env.initial)
  in
  List.iter
    per_context
    ~f:(print_solver_env_for_one_context ~sys_bindings_from_current_system)
;;

let term =
  let+ builder = Common.Builder.term
  and+ context_name =
    context_term
      ~doc:
        "Generate the lockdir associated with this context (the default context will be \
         used if this is omitted)"
  and+ all_contexts =
    Arg.(
      value & flag & info [ "all-contexts" ] ~doc:"Generate the lockdir for all contexts")
  and+ version_preference = Version_preference.term
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
    print_solver_env
      ~context_name
      ~all_contexts
      ~version_preference
      ~dont_poll_system_solver_variables)
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
