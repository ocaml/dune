open Import
module Lock_dir = Dune_pkg.Lock_dir
module Fetch = Dune_pkg.Fetch
module Opam_repo = Dune_pkg.Opam_repo
module Repository_id = Dune_pkg.Repository_id

let context_term =
  Arg.(
    value
    & opt (some Arg.context_name) None
    & info
        [ "context" ]
        ~docv:"CONTEXT"
        ~doc:
          "Generate the lockdir associated with this context (the default context will \
           be used if this is omitted)")
;;

module Version_preference = struct
  include Dune_pkg.Version_preference

  let term =
    let all_strings = List.map all_by_string ~f:fst in
    let doc =
      sprintf
        "Whether to prefer the newest compatible version of a package or the oldest \
         compatible version of packages while solving dependencies. This overrides any \
         setting in the current workspace. The default is %s."
        (to_string default)
    in
    let docv = String.concat ~sep:"|" all_strings |> sprintf "(%s)" in
    Arg.(
      value
      & opt (some (enum all_by_string)) None
      & info [ "version-preference" ] ~doc ~docv)
  ;;

  let choose ~from_arg ~from_context =
    match from_arg, from_context with
    | Some from_arg, _ -> from_arg
    | None, Some from_context -> from_context
    | None, None -> default
  ;;
end

module Per_context = struct
  type t =
    { lock_dir_path : Path.Source.t
    ; version_preference : Version_preference.t
    ; solver_env : Dune_pkg.Solver_env.t
    ; context_common : Dune_rules.Workspace.Context.Common.t
    ; repos :
        Dune_pkg.Pkg_workspace.Repository.t Dune_pkg.Pkg_workspace.Repository.Name.Map.t
    }

  let repositories_of_workspace (workspace : Workspace.t) =
    List.map workspace.repos ~f:(fun repo ->
      Dune_pkg.Pkg_workspace.Repository.name repo, repo)
    |> Dune_pkg.Pkg_workspace.Repository.Name.Map.of_list_exn
  ;;

  let choose ~context_name_arg ~all_contexts_arg ~version_preference_arg =
    let open Fiber.O in
    match context_name_arg, all_contexts_arg with
    | Some _, true ->
      User_error.raise [ Pp.text "--context and --all-contexts are mutually exclusive" ]
    | context_name_opt, false ->
      let+ workspace = Memo.run (Workspace.workspace ()) in
      let context_name =
        Option.value context_name_opt ~default:Dune_engine.Context_name.default
      in
      let context =
        (* TODO this doesn't work for target contexts defined by cross compilation *)
        List.find workspace.contexts ~f:(fun context ->
          Dune_engine.Context_name.equal (Workspace.Context.name context) context_name)
      in
      (match context with
       | None ->
         User_error.raise
           [ Pp.textf
               "Unknown build context: %s"
               (Dune_engine.Context_name.to_string context_name |> String.maybe_quoted)
           ]
       | Some
           (Default
             { lock
             ; version_preference = version_preference_context
             ; solver_env
             ; base = context_common
             ; _
             }) ->
         [ { lock_dir_path = Option.value lock ~default:Lock_dir.default_path
           ; version_preference =
               Version_preference.choose
                 ~from_arg:version_preference_arg
                 ~from_context:version_preference_context
           ; solver_env = Option.value solver_env ~default:Dune_pkg.Solver_env.default
           ; context_common
           ; repos = repositories_of_workspace workspace
           }
         ]
       | Some (Opam _) ->
         User_error.raise
           [ Pp.textf
               "Unexpected opam build context: %s"
               (Dune_engine.Context_name.to_string context_name |> String.maybe_quoted)
           ])
    | None, true ->
      let+ workspace = Memo.run (Workspace.workspace ()) in
      List.filter_map workspace.contexts ~f:(function
        | Workspace.Context.Default
            { lock
            ; version_preference = version_preference_context
            ; base = context_common
            ; solver_env
            } ->
          let lock_dir_path = Option.value lock ~default:Dune_pkg.Lock_dir.default_path in
          Some
            { lock_dir_path
            ; version_preference =
                Version_preference.choose
                  ~from_arg:version_preference_arg
                  ~from_context:version_preference_context
            ; context_common
            ; solver_env = Option.value solver_env ~default:Dune_pkg.Solver_env.default
            ; repos = repositories_of_workspace workspace
            }
        | Opam _ -> None)
  ;;

  let contexts_with_dup_lock_dir_paths ts =
    List.map ts ~f:(fun { lock_dir_path; context_common; _ } ->
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
             "Refusing to proceed as multiple selected contexts would create a lock dir \
              at the same path."
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
end

module Print_solver_env = struct
  (* Add the opam system variables derived from the current system into a
     solver environment configured in a build context. If variables derived
     from the current system are also present in the solver environment
     configured in the build context, an error is raised unless they both have
     the same value (in which case that value is used). *)
  let merge_current_system_bindings_into_solver_env_from_context
    ~context_name
    ~solver_env_from_context
    ~sys_bindings_from_current_system
    ~use_env_from_current_system
    =
    let sys =
      match
        Dune_pkg.Solver_env.(
          Variable.Sys.Bindings.union
            (sys solver_env_from_context)
            sys_bindings_from_current_system)
      with
      | Ok solver_env -> solver_env
      | Error
          (`Var_in_both_with_different_values
            (var, value_from_context, value_from_system)) ->
        let hints =
          if use_env_from_current_system
          then
            [ Pp.text
                "This can happen if --use-env-from-current-system is passed and the \
                 build context specifies environment variables which differ from those \
                 of the current system."
            ]
          else []
        in
        User_error.raise
          ~hints
          [ Pp.textf
              "Can't create solver environment for context %s"
              (String.maybe_quoted @@ Dune_engine.Context_name.to_string context_name)
          ; Pp.textf
              "Conflicting values for system environment variable: %s"
              (String.maybe_quoted @@ Dune_pkg.Solver_env.Variable.Sys.to_string var)
          ; Pp.textf
              "The value inferred from the current system is: %s"
              (String.maybe_quoted value_from_system)
          ; Pp.textf
              "The value configured in the build context (%s) is: %s"
              (String.maybe_quoted @@ Dune_engine.Context_name.to_string context_name)
              (String.maybe_quoted value_from_context)
          ]
    in
    Dune_pkg.Solver_env.set_sys solver_env_from_context sys
  ;;

  let print_solver_env_for_one_context
    ~sys_bindings_from_current_system
    ~use_env_from_current_system
    { Per_context.solver_env = solver_env_from_context
    ; context_common = { name = context_name; _ }
    ; _
    }
    =
    let solver_env =
      merge_current_system_bindings_into_solver_env_from_context
        ~context_name
        ~solver_env_from_context
        ~sys_bindings_from_current_system
        ~use_env_from_current_system
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
    ~use_env_from_current_system
    =
    let open Fiber.O in
    let+ per_context =
      Per_context.choose
        ~context_name_arg:context_name
        ~all_contexts_arg:all_contexts
        ~version_preference_arg:version_preference
    and+ sys_bindings_from_current_system =
      if use_env_from_current_system
      then Dune_pkg.Sys_poll.sys_bindings ~path:(Env_path.path Stdune.Env.initial)
      else Fiber.return Dune_pkg.Solver_env.Variable.Sys.Bindings.empty
    in
    List.iter
      per_context
      ~f:
        (print_solver_env_for_one_context
           ~sys_bindings_from_current_system
           ~use_env_from_current_system)
  ;;

  let term =
    let+ (common : Common.t) = Common.term
    and+ context_name = context_term
    and+ all_contexts =
      Arg.(
        value
        & flag
        & info [ "all-contexts" ] ~doc:"Generate the lockdir for all contexts")
    and+ version_preference = Version_preference.term
    and+ use_env_from_current_system =
      Arg.(
        value
        & flag
        & info
            [ "use-env-from-current-system" ]
            ~doc:
              "Set opam system environment variables based on the current system when \
               solving dependencies. This will restrict packages to just those which can \
               be installed on the current system. Note that this will mean that the \
               generated lockdir may not be compatible with other systems. If the build \
               context(s) specify system environment variables then the solver will use \
               the union of the environment variables inferred from the current system \
               and the environment variables set in the build context(s). If the current \
               system and the build context(s) disagree on the value of an environment \
               variable then an error is raised.")
    in
    let common = Common.forbid_builds common in
    let config = Common.init common in
    Scheduler.go ~common ~config (fun () ->
      print_solver_env
        ~context_name
        ~all_contexts
        ~version_preference
        ~use_env_from_current_system)
  ;;

  let info =
    let doc =
      "Print an s-expression describing the environment that would be used to solve \
       dependencies and then exit without attempting to solve the dependencies or \
       generate the lockfile. Intended to be used to debug situations where no solution \
       can be found to a project's dependencies."
    in
    Cmd.info "print-solver-env" ~doc
  ;;

  let command = Cmd.v info term
end

module Lock = struct
  module Opam_repository_path = struct
    let term =
      let dune_path =
        let parser s =
          s
          |> Path.External.of_filename_relative_to_initial_cwd
          |> Path.external_
          |> Result.ok
        in
        let printer pf p = Pp.to_fmt pf (Path.pp p) in
        Arg.conv (parser, printer)
      in
      Arg.(
        value
        & opt (some dune_path) None
        & info
            [ "opam-repository-path" ]
            ~docv:"PATH"
            ~doc:
              "Path to a local opam repository. This should be a directory containing a \
               valid opam repository such as the one at \
               https://github.com/ocaml/opam-repository.")
    ;;
  end

  module Opam_repository_url = struct
    let term =
      let parser s =
        match OpamUrl.parse_opt s with
        | Some url -> Ok url
        | None -> Error (`Msg "URL can't be parsed")
      in
      let printer pf u = Pp.to_fmt pf (Pp.text (OpamUrl.to_string u)) in
      let opam_url = Arg.conv (parser, printer) in
      Arg.(
        value
        & opt (some opam_url) None
        & info
            [ "opam-repository-url" ]
            ~docv:"URL"
            ~doc:
              "URL of opam repository to download. Can be either a git repository or a \
               link to the tarball of a repository.")
    ;;
  end

  let get_repos repos solver_env ~opam_repository_path ~opam_repository_url =
    let open Fiber.O in
    match opam_repository_path, opam_repository_url with
    | Some _, Some _ ->
      (* in theory you can set both, but how to prioritize them? *)
      User_error.raise [ Pp.text "Can't specify both path and URL to an opam-repository" ]
    | Some path, None ->
      let repo_id = Repository_id.of_path path in
      Fiber.return @@ [ Opam_repo.of_opam_repo_dir_path ~source:None ~repo_id path ]
    | None, Some url ->
      let repo = Fetch.Opam_repository.of_url url in
      let+ opam_repository = Fetch.Opam_repository.path repo in
      (match opam_repository with
       | Ok { path; repo_id } ->
         [ Opam_repo.of_opam_repo_dir_path
             ~source:(Some (OpamUrl.to_string url))
             ~repo_id
             path
         ]
       | Error _ ->
         User_error.raise
           [ Pp.text "Can't determine the location of the opam-repository" ])
    | None, None ->
      (* read from workspace *)
      Dune_pkg.Solver_env.repos solver_env
      |> Fiber.parallel_map ~f:(fun name ->
        match Dune_pkg.Pkg_workspace.Repository.Name.Map.find repos name with
        | None ->
          (* TODO: have loc for this failure? *)
          User_error.raise
            [ Pp.textf "Repository '%s' is not a known repository"
              @@ Dune_pkg.Pkg_workspace.Repository.Name.to_string name
            ]
        | Some repo ->
          let url = Dune_pkg.Pkg_workspace.Repository.opam_url repo in
          let repo = Fetch.Opam_repository.of_url url in
          let+ opam_repository = Fetch.Opam_repository.path repo in
          (match opam_repository with
           | Ok { path; repo_id } ->
             Opam_repo.of_opam_repo_dir_path
               ~source:(Some (OpamUrl.to_string url))
               ~repo_id
               path
           | Error _ ->
             User_error.raise
               [ Pp.textf "Can't determine the location of the opam-repository '%s'"
                 @@ Dune_pkg.Pkg_workspace.Repository.Name.to_string name
               ]))
  ;;

  let solve
    per_context
    ~opam_repository_path
    ~opam_repository_url
    ~sys_bindings_from_current_system
    ~use_env_from_current_system
    =
    let open Fiber.O in
    Per_context.check_for_dup_lock_dir_paths per_context;
    (* a list of thunks that will perform all the file IO side
       effects after performing validation so that if materializing any
       lockdir would fail then no side effect takes place. *)
    (let* local_packages =
       let+ project =
         let+ source_dir = Memo.run (Source_tree.root ()) in
         Source_tree.Dir.project source_dir
       in
       Dune_project.packages project
       |> Package.Name.Map.map ~f:(fun pkg ->
         let opam_file = Package.to_opam_file pkg in
         let file =
           Path.source
           @@
           match pkg.has_opam_file with
           | Generated | Exists false -> Dune_project.file project
           | Exists true -> pkg.opam_file
         in
         { Opam_repo.With_file.opam_file; file })
     in
     let+ solutions =
       Fiber.parallel_map
         per_context
         ~f:
           (fun
             { Per_context.lock_dir_path
             ; version_preference
             ; repos
             ; solver_env = solver_env_from_context
             ; context_common = { name = context_name; _ }
             }
           ->
           let solver_env =
             Print_solver_env.merge_current_system_bindings_into_solver_env_from_context
               ~context_name
               ~solver_env_from_context
               ~sys_bindings_from_current_system
               ~use_env_from_current_system
           in
           let+ repos =
             get_repos repos solver_env ~opam_repository_path ~opam_repository_url
           in
           match
             Console.Status_line.with_overlay
               (Constant (Pp.text "Solving for Build Plan"))
               ~f:(fun () ->
                 Dune_pkg.Opam_solver.solve_lock_dir
                   solver_env
                   version_preference
                   repos
                   ~local_packages)
           with
           | Error (`Diagnostic_message message) -> Error (context_name, message)
           | Ok { Dune_pkg.Opam_solver.Solver_result.summary; lock_dir; files } ->
             let summary_message =
               Dune_pkg.Opam_solver.Summary.selected_packages_message
                 summary
                 ~lock_dir_path
               |> User_message.pp
             in
             Ok
               ( Lock_dir.Write_disk.prepare ~lock_dir_path ~files lock_dir
               , summary_message ))
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
      let write_disk_list, summary_pps = List.split write_disks_with_summaries in
      Dune_console.print summary_pps;
      (* All the file IO side effects happen here: *)
      List.iter write_disk_list ~f:Lock_dir.Write_disk.commit
  ;;

  let lock
    ~context_name
    ~all_contexts
    ~version_preference
    ~use_env_from_current_system
    ~opam_repository_path
    ~opam_repository_url
    =
    let open Fiber.O in
    let* per_context =
      Per_context.choose
        ~context_name_arg:context_name
        ~all_contexts_arg:all_contexts
        ~version_preference_arg:version_preference
    and* sys_bindings_from_current_system =
      if use_env_from_current_system
      then Dune_pkg.Sys_poll.sys_bindings ~path:(Env_path.path Stdune.Env.initial)
      else Fiber.return Dune_pkg.Solver_env.Variable.Sys.Bindings.empty
    in
    solve
      per_context
      ~opam_repository_path
      ~opam_repository_url
      ~sys_bindings_from_current_system
      ~use_env_from_current_system
  ;;

  let term =
    let+ (common : Common.t) = Common.term
    and+ opam_repository_path = Opam_repository_path.term
    and+ opam_repository_url = Opam_repository_url.term
    and+ context_name = context_term
    and+ all_contexts =
      Arg.(
        value
        & flag
        & info [ "all-contexts" ] ~doc:"Generate the lockdir for all contexts")
    and+ version_preference = Version_preference.term
    and+ use_env_from_current_system =
      Arg.(
        value
        & flag
        & info
            [ "use-env-from-current-system" ]
            ~doc:
              "Set opam system environment variables based on the current system when \
               solving dependencies. This will restrict packages to just those which can \
               be installed on the current system. Note that this will mean that the \
               generated lockdir may not be compatible with other systems. If the build \
               context(s) specify system environment variables then the solver will use \
               the union of the environment variables inferred from the current system \
               and the environment variables set in the build context(s). If the current \
               system and the build context(s) disagree on the value of an environment \
               variable then an error is raised.")
    in
    let common = Common.forbid_builds common in
    let config = Common.init common in
    Scheduler.go ~common ~config (fun () ->
      lock
        ~context_name
        ~all_contexts
        ~version_preference
        ~use_env_from_current_system
        ~opam_repository_path
        ~opam_repository_url)
  ;;

  let info =
    let doc = "Create a lockfile" in
    Cmd.info "lock" ~doc
  ;;

  let command = Cmd.v info term
end

let info =
  let doc = "Experimental package management" in
  let man =
    [ `S "DESCRIPTION"
    ; `P {|Commands for doing package management with dune|}
    ; `Blocks Common.help_secs
    ]
  in
  Cmd.info "pkg" ~doc ~man
;;

let group = Cmd.group info [ Lock.command; Print_solver_env.command ]
