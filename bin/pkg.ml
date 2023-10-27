open Import
module Lock_dir = Dune_pkg.Lock_dir
module Fetch = Dune_pkg.Fetch
module Opam_repo = Dune_pkg.Opam_repo
module Repository_id = Dune_pkg.Repository_id

let context_term ~doc =
  Arg.(value & opt (some Arg.context_name) None & info [ "context" ] ~docv:"CONTEXT" ~doc)
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
    ; solver_sys_vars : Dune_pkg.Solver_env.Variable.Sys.Bindings.t option
    ; repositories : Dune_pkg.Pkg_workspace.Repository.Name.t list
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
             ; solver_sys_vars
             ; repositories
             ; base = context_common
             ; _
             }) ->
         [ { lock_dir_path = Option.value lock ~default:Lock_dir.default_path
           ; version_preference =
               Version_preference.choose
                 ~from_arg:version_preference_arg
                 ~from_context:version_preference_context
           ; solver_sys_vars
           ; repositories
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
            ; solver_sys_vars
            ; repositories
            } ->
          let lock_dir_path = Option.value lock ~default:Dune_pkg.Lock_dir.default_path in
          Some
            { lock_dir_path
            ; version_preference =
                Version_preference.choose
                  ~from_arg:version_preference_arg
                  ~from_context:version_preference_context
            ; context_common
            ; solver_sys_vars
            ; repositories
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

(* The system environment variables used by the solver are taken from the
   current system by default but can be overridden by the build context. *)
let solver_env_variables ~solver_sys_vars_from_context ~sys_bindings_from_current_system =
  match solver_sys_vars_from_context with
  | None -> sys_bindings_from_current_system
  | Some solver_env_variables ->
    Dune_pkg.Solver_env.Variable.Sys.Bindings.extend
      sys_bindings_from_current_system
      solver_env_variables
;;

module Print_solver_env = struct
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
          "Generate the lockdir associated with this context (the default context will \
           be used if this is omitted)"
    and+ all_contexts =
      Arg.(
        value
        & flag
        & info [ "all-contexts" ] ~doc:"Generate the lockdir for all contexts")
    and+ version_preference = Version_preference.term
    and+ dont_poll_system_solver_variables =
      Arg.(
        value
        & flag
        & info
            [ "dont-poll-system-solver-variables" ]
            ~doc:
              "Don't derive system solver variables from the current system. Values \
               assigned to these variables in build contexts will still be used. Note \
               that Opam filters that depend on unset variables resolve to the value \
               \"undefined\" which is treated as false. For example if a dependency has \
               a filter `{os = \"linux\"}` and the variable \"os\" is unset, the \
               dependency will be excluded. ")
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

  let get_repos repos ~opam_repository_path ~opam_repository_url ~repositories =
    let open Fiber.O in
    match opam_repository_path, opam_repository_url with
    | Some _, Some _ ->
      (* in theory you can set both, but how to prioritize them? *)
      User_error.raise [ Pp.text "Can't specify both path and URL to an opam-repository" ]
    | Some path, None ->
      let repo_id = Repository_id.of_path path in
      Fiber.return @@ [ Opam_repo.of_opam_repo_dir_path ~source:None ~repo_id path ]
    | None, Some (url : OpamUrl.t) ->
      let+ opam_repo = Opam_repo.of_git_repo ~repo_id:None ~source:url.path in
      [ opam_repo ]
    | None, None ->
      repositories
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
          Opam_repo.of_git_repo ~repo_id:None ~source:url.path)
  ;;

  let find_local_packages =
    let open Fiber.O in
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
  ;;

  let pp_packages packages =
    Pp.enumerate
      packages
      ~f:(fun { Lock_dir.Pkg.info = { Lock_dir.Pkg_info.name; version; _ }; _ } ->
        Pp.verbatim (Package_name.to_string name ^ "." ^ version))
  ;;

  let solve
    per_context
    ~opam_repository_path
    ~opam_repository_url
    ~sys_bindings_from_current_system
    ~experimental_translate_opam_filters
    =
    let open Fiber.O in
    Per_context.check_for_dup_lock_dir_paths per_context;
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
             get_repos repos ~opam_repository_path ~opam_repository_url ~repositories
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
                 ~local_packages
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
          "Generate the lockdir associated with this context (the default context will \
           be used if this is omitted)"
    and+ all_contexts =
      Arg.(
        value
        & flag
        & info [ "all-contexts" ] ~doc:"Generate the lockdir for all contexts")
    and+ version_preference = Version_preference.term
    and+ dont_poll_system_solver_variables =
      Arg.(
        value
        & flag
        & info
            [ "dont-poll-system-solver-variables" ]
            ~doc:
              "Don't derive system solver variables from the current system. Values \
               assigned to these variables in build contexts will still be used. Note \
               that Opam filters that depend on unset variables resolve to the value \
               \"undefined\" which is treated as false. For example if a dependency has \
               a filter `{os = \"linux\"}` and the variable \"os\" is unset, the \
               dependency will be excluded. ")
    and+ experimental_translate_opam_filters =
      Arg.(
        value
        & flag
        & info
            [ "experimental-translate-opam-filters" ]
            ~doc:
              "Translate Opam filters into Dune's \"Slang\" DSL. This will eventually be \
               enabled by default but is currently opt-in as we expect to make major \
               changes to it in the future. Without this flag all conditional commands \
               and terms in Opam files are included unconditionally.")
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
        ~experimental_translate_opam_filters)
  ;;

  let info =
    let doc = "Create a lockfile" in
    Cmd.info "lock" ~doc
  ;;

  let command = Cmd.v info term
end

module Outdated = struct
  let find_outdated_packages
    ~context_name_arg
    ~all_contexts_arg
    ~opam_repository_path
    ~opam_repository_url
    ~transitive
    ()
    =
    let open Fiber.O in
    let+ pps, not_founds =
      Per_context.choose ~context_name_arg ~all_contexts_arg ~version_preference_arg:None
      >>= Fiber.parallel_map
            ~f:
              (fun
                { Per_context.lock_dir_path
                ; version_preference = _
                ; repos
                ; solver_sys_vars = _
                ; context_common = _
                ; repositories
                }
              ->
              let* repos =
                Lock.get_repos
                  repos
                  ~opam_repository_path
                  ~opam_repository_url
                  ~repositories
              and+ local_packages = Lock.find_local_packages in
              let lock_dir = Lock_dir.read_disk lock_dir_path in
              let+ results =
                Dune_pkg_outdated.find ~repos ~local_packages lock_dir.packages
              in
              ( Dune_pkg_outdated.pp ~transitive ~lock_dir_path results
              , ( Dune_pkg_outdated.packages_that_were_not_found results
                  |> Package_name.Set.of_list
                  |> Package_name.Set.to_list
                , lock_dir_path
                , repos ) ))
      >>| List.split
    in
    (match pps with
     | [ _ ] -> Console.print pps
     | _ -> Console.print [ Pp.enumerate ~f:Fun.id pps ]);
    let error_messages =
      List.filter_map not_founds ~f:(function
        | [], _, _ -> None
        | packages, lock_dir_path, repos ->
          Pp.concat
            ~sep:Pp.space
            [ Pp.textf
                "When checking %s, the following packages:"
                (Path.Source.to_string_maybe_quoted lock_dir_path)
              |> Pp.hovbox
            ; Pp.concat
                ~sep:Pp.space
                [ Pp.enumerate packages ~f:(fun name ->
                    Dune_lang.Package_name.to_string name |> Pp.verbatim)
                ; Pp.text "were not found in the following opam repositories:"
                  |> Pp.hovbox
                ; Pp.enumerate repos ~f:(fun repo ->
                    Opam_repo.serializable repo
                    |> Dyn.option Opam_repo.Serializable.to_dyn
                    |> Dyn.pp)
                ]
              |> Pp.vbox
            ]
          |> Pp.hovbox
          |> Option.some)
    in
    match error_messages with
    | [] -> ()
    | error_messages ->
      User_error.raise (Pp.text "Some packages could not be found." :: error_messages)
  ;;

  let term =
    let+ builder = Common.Builder.term
    and+ context_name_arg =
      context_term ~doc:"Check for outdated packages in this context"
    and+ all_contexts_arg =
      Arg.(
        value
        & flag
        & info [ "all-contexts" ] ~doc:"Check for outdated packages in all contexts")
    and+ opam_repository_path = Lock.Opam_repository_path.term
    and+ opam_repository_url = Lock.Opam_repository_url.term
    and+ transitive =
      Arg.(
        value
        & flag
        & info
            [ "transitive" ]
            ~doc:"Check for outdated packages in transitive dependencies")
    in
    let builder = Common.Builder.forbid_builds builder in
    let common, config = Common.init builder in
    Scheduler.go ~common ~config
    @@ find_outdated_packages
         ~context_name_arg
         ~all_contexts_arg
         ~opam_repository_path
         ~opam_repository_url
         ~transitive
  ;;

  let info =
    let doc = "Check for outdated packages" in
    let man =
      [ `S "DESCRIPTION"
      ; `P
          "List packages in from lock directory that have newer versions available. By \
           default, only direct dependencies are checked. The $(b,--transitive) flag can \
           be used to check transitive dependencies as well."
      ; `P "For example:"
      ; `Pre "    \\$ dune pkg outdated"
      ; `Noblank
      ; `Pre "    1/2 packages in dune.lock are outdated."
      ; `Noblank
      ; `Pre "    - ocaml 4.14.1 < 5.1.0"
      ; `Noblank
      ; `Pre "    - dune 3.7.1 < 3.11.0"
      ]
    in
    Cmd.info "outdated" ~doc ~man
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

let group = Cmd.group info [ Lock.command; Print_solver_env.command; Outdated.command ]
