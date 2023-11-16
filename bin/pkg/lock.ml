open Import
open Pkg_common
module Package_version = Dune_pkg.Package_version
module Opam_repo = Dune_pkg.Opam_repo
module Repository_id = Dune_pkg.Repository_id
module Lock_dir = Dune_pkg.Lock_dir

let contexts_with_dup_lock_dir_paths ts =
  List.map ts ~f:(fun { Per_context.lock_dir_path; context_common; _ } ->
    lock_dir_path, context_common)
  |> Path.Source.Map.of_list_multi
  |> Path.Source.Map.to_list
  |> List.find_opt ~f:(fun (_, context_commons) -> List.length context_commons > 1)
;;

let check_for_dup_lock_dir_paths ts =
  contexts_with_dup_lock_dir_paths ts
  |> Option.iter ~f:(fun (lock_dir_path, context_commons) ->
    let loc = (List.hd context_commons : Workspace.Context.Common.t).loc in
    User_error.raise
      ~loc
      ([ Pp.text
           "Refusing to proceed as multiple selected contexts would create a lock dir at \
            the same path."
       ; Pp.textf
           "These contexts all create a lock dir: %s"
           (Path.Source.to_string_maybe_quoted lock_dir_path)
       ]
       @ List.map context_commons ~f:(fun (c : Dune_rules.Workspace.Context.Common.t) ->
         Pp.textf
           "- %s (defined at %s)"
           (Context_name.to_string c.name |> String.maybe_quoted)
           (Loc.to_file_colon_line c.loc))))
;;

let solve
  per_context
  ~opam_repository_path
  ~opam_repository_url
  ~update_opam_repositories
  ~sys_bindings_from_current_system
  ~experimental_translate_opam_filters
  =
  let open Fiber.O in
  check_for_dup_lock_dir_paths per_context;
  (* a list of thunks that will perform all the file IO side
     effects after performing validation so that if materializing any
     lockdir would fail then no side effect takes place. *)
  (let* local_packages = find_local_packages in
   let+ solutions =
     Fiber.parallel_map
       per_context
       ~f:
         (fun
           { Per_context.lock_dir_path
           ; version_preference
           ; repos
           ; solver_sys_vars = solver_sys_vars_from_context
           ; context_common = { name = context_name; _ }
           ; repositories
           }
         ->
         let solver_env =
           Dune_pkg.Solver_env.create
             ~sys:
               (solver_env_variables
                  ~solver_sys_vars_from_context
                  ~sys_bindings_from_current_system)
         in
         let* repos =
           get_repos
             repos
             ~opam_repository_path
             ~opam_repository_url
             ~repositories
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
             Dune_pkg.Opam_solver.solve_lock_dir
               solver_env
               version_preference
               repos
               ~local_packages:
                 (Package_name.Map.map
                    local_packages
                    ~f:Dune_pkg.Local_package.for_solver)
               ~experimental_translate_opam_filters)
         >>| function
         | Error (`Diagnostic_message message) -> Error (context_name, message)
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
                    Pp.tag User_message.Style.Warning
                    @@ Pp.text "(no dependencies to lock)"
                  | packages -> pp_packages packages)
               ]
           in
           Ok (Lock_dir.Write_disk.prepare ~lock_dir_path ~files lock_dir, summary_message))
   in
   Result.List.all solutions)
  >>| function
  | Error (context_name, message) ->
    User_error.raise
      [ Pp.textf
          "Unable to solve dependencies in build context: %s"
          (Dune_engine.Context_name.to_string context_name |> String.maybe_quoted)
      ; message
      ]
  | Ok write_disks_with_summaries ->
    let write_disk_list, summary_messages = List.split write_disks_with_summaries in
    List.iter summary_messages ~f:Console.print_user_message;
    (* All the file IO side effects happen here: *)
    List.iter write_disk_list ~f:Lock_dir.Write_disk.commit
;;

let lock
  ~context_name
  ~all_contexts
  ~dont_poll_system_solver_variables
  ~version_preference
  ~opam_repository_path
  ~opam_repository_url
  ~update_opam_repositories
  ~experimental_translate_opam_filters
  =
  let open Fiber.O in
  let* per_context =
    Per_context.choose
      ~context_name_arg:context_name
      ~all_contexts_arg:all_contexts
      ~version_preference_arg:version_preference
  and* sys_bindings_from_current_system =
    if dont_poll_system_solver_variables
    then Fiber.return Dune_pkg.Solver_env.Variable.Sys.Bindings.empty
    else Dune_pkg.Sys_poll.sys_bindings ~path:(Env_path.path Stdune.Env.initial)
  in
  solve
    per_context
    ~opam_repository_path
    ~opam_repository_url
    ~update_opam_repositories
    ~sys_bindings_from_current_system
    ~experimental_translate_opam_filters
;;

let term =
  let+ builder = Common.Builder.term
  and+ opam_repository_path = Opam_repository_path.term
  and+ opam_repository_url = Opam_repository_url.term
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
  and+ experimental_translate_opam_filters =
    Arg.(
      value
      & flag
      & info
          [ "experimental-translate-opam-filters" ]
          ~doc:
            "Translate Opam filters into Dune's \"Slang\" DSL. This will eventually be \
             enabled by default but is currently opt-in as we expect to make major \
             changes to it in the future. Without this flag all conditional commands and \
             terms in Opam files are included unconditionally.")
  and+ skip_update =
    Arg.(
      value
      & flag
      & info
          [ "skip-update" ]
          ~doc:
            "Do not fetch updates of opam repositories, will use the cached opam \
             metadata. This allows offline use if the repositories are cached locally.")
  in
  let builder = Common.Builder.forbid_builds builder in
  let common, config = Common.init builder in
  Scheduler.go ~common ~config (fun () ->
    lock
      ~context_name
      ~all_contexts
      ~dont_poll_system_solver_variables
      ~version_preference
      ~opam_repository_path
      ~opam_repository_url
      ~update_opam_repositories:(not skip_update)
      ~experimental_translate_opam_filters)
;;

let info =
  let doc = "Create a lockfile" in
  Cmd.info "lock" ~doc
;;

let command = Cmd.v info term
