open Import
module Lock_dir = Dune_pkg.Lock_dir
module Fetch = Dune_pkg.Fetch

module Opam_repository = struct
  type t = { url : OpamUrl.t }

  let of_url url = { url }

  let default =
    of_url @@ OpamUrl.of_string "https://opam.ocaml.org/index.tar.gz"

  let is_archive name =
    List.exists
      ~f:(fun suffix -> String.is_suffix ~suffix name)
      [ ".tar.gz"; ".tgz"; ".tar.bz2"; ".tbz"; ".zip" ]

  let path =
    let open Fiber.O in
    let ( / ) = Filename.concat in
    fun { url } ->
      Fiber.of_thunk @@ fun () ->
      let target_dir =
        Xdg.cache_dir (Lazy.force Dune_util.xdg) / "dune/opam-repository"
      in
      let target = target_dir |> Path.External.of_string |> Path.external_ in
      let unpack = url |> OpamUrl.to_string |> is_archive in
      let+ res = Fetch.fetch ~unpack ~checksum:None ~target url in
      match res with
      | Ok () -> Ok target
      | Error _ as failure -> failure
end

module Lock = struct
  module Opam_repository_path = struct
    let term =
      let dune_path =
        let parser s =
          s |> Path.External.of_filename_relative_to_initial_cwd
          |> Path.external_ |> Result.ok
        in
        let printer pf p = Pp.to_fmt pf (Path.pp p) in
        Arg.conv (parser, printer)
      in
      Arg.(
        value
        & opt (some dune_path) None
        & info [ "opam-repository-path" ] ~docv:"PATH"
            ~doc:
              "Path to a local opam repository. This should be a directory \
               containing a valid opam repository such as the one at \
               https://github.com/ocaml/opam-repository.")
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
        & info [ "opam-repository-url" ] ~docv:"URL"
            ~doc:
              "URL of opam repository to download. Can be either a git \
               repository or a link to the tarball of a repository.")
  end

  module Version_preference = struct
    include Dune_pkg.Version_preference

    let term =
      let all_strings = List.map all_by_string ~f:fst in
      let doc =
        sprintf
          "Whether to prefer the newest compatible version of a package or the \
           oldest compatible version of packages while solving dependencies. \
           This overrides any setting in the current workspace. The default is \
           %s."
          (to_string default)
      in
      let docv = String.concat ~sep:"|" all_strings |> sprintf "(%s)" in
      Arg.(
        value
        & opt (some (enum all_by_string)) None
        & info [ "version-preference" ] ~doc ~docv)

    let choose ~from_arg ~from_context =
      match (from_arg, from_context) with
      | Some from_arg, _ -> from_arg
      | None, Some from_context -> from_context
      | None, None -> default
  end

  (* Converts the package table found inside a [Dune_project.t] into the
     package table expected by the dependency solver *)
  let opam_file_map_of_dune_package_map
      (dune_package_map : Package.t Package.Name.Map.t) :
      OpamFile.OPAM.t OpamTypes.name_map =
    Package.Name.Map.to_list_map dune_package_map
      ~f:(fun dune_package_name dune_package ->
        let opam_package_name =
          Package.Name.to_opam_package_name dune_package_name
        in
        let opam_file = Package.to_opam_file dune_package in
        (opam_package_name, opam_file))
    |> OpamPackage.Name.Map.of_list

  module Per_context = struct
    type t =
      { lock_dir_path : Path.Source.t
      ; version_preference : Version_preference.t
      ; solver_env : Dune_pkg.Solver_env.t
      ; context_common : Dune_rules.Workspace.Context.Common.t
      }

    let choose ~context_name_arg ~all_contexts_arg ~version_preference_arg =
      let open Fiber.O in
      match (context_name_arg, all_contexts_arg) with
      | Some _, true ->
        User_error.raise
          [ Pp.text "--context and --all-contexts are mutually exclusive" ]
      | context_name_opt, false -> (
        let+ workspace = Memo.run (Workspace.workspace ()) in
        let context_name =
          Option.value context_name_opt
            ~default:Dune_engine.Context_name.default
        in
        let context =
          List.find workspace.contexts ~f:(fun context ->
              Dune_engine.Context_name.equal
                (Workspace.Context.name context)
                context_name)
        in
        match context with
        | None ->
          User_error.raise
            [ Pp.textf "Unknown build context: %s"
                (Dune_engine.Context_name.to_string context_name
                |> String.maybe_quoted)
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
                Version_preference.choose ~from_arg:version_preference_arg
                  ~from_context:version_preference_context
            ; solver_env =
                Option.value solver_env ~default:Dune_pkg.Solver_env.default
            ; context_common
            }
          ]
        | Some (Opam _) ->
          User_error.raise
            [ Pp.textf "Unexpected opam build context: %s"
                (Dune_engine.Context_name.to_string context_name
                |> String.maybe_quoted)
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
            let lock_dir_path =
              Option.value lock ~default:Dune_pkg.Lock_dir.default_path
            in
            Some
              { lock_dir_path
              ; version_preference =
                  Version_preference.choose ~from_arg:version_preference_arg
                    ~from_context:version_preference_context
              ; context_common
              ; solver_env =
                  Option.value solver_env ~default:Dune_pkg.Solver_env.default
              }
          | Opam _ -> None)

    let contexts_with_dup_lock_dir_paths ts =
      List.map ts ~f:(fun { lock_dir_path; context_common; _ } ->
          (lock_dir_path, context_common))
      |> Path.Source.Map.of_list_multi |> Path.Source.Map.to_list
      |> List.find_opt ~f:(fun (_, context_commons) ->
             List.length context_commons > 1)

    let check_for_dup_lock_dir_paths ts =
      contexts_with_dup_lock_dir_paths ts
      |> Option.iter ~f:(fun (lock_dir_path, context_commons) ->
             let loc =
               (List.hd context_commons : Workspace.Context.Common.t).loc
             in
             User_error.raise ~loc
               ([ Pp.text
                    "Refusing to proceed as multiple selected contexts would \
                     create a lock dir at the same path."
                ; Pp.textf "These contexts all create a lock dir: %s"
                    (Path.Source.to_string_maybe_quoted lock_dir_path)
                ]
               @ List.map context_commons
                   ~f:(fun (c : Dune_rules.Workspace.Context.Common.t) ->
                     Pp.textf "- %s (defined at %s)"
                       (Context_name.to_string c.name |> String.maybe_quoted)
                       (Loc.to_file_colon_line c.loc))))
  end

  (* Add the opam system variables derived from the current system into a
     solver environment configured in a build context. If variables derived
     from the current system are also present in the solver environment
     configured in the build context, an error is raised unless they both have
     the same value (in which case that value is used). *)
  let merge_current_system_bindings_into_solver_env_from_context ~context_name
      ~solver_env_from_context ~sys_bindings_from_current_system =
    let sys =
      match
        Dune_pkg.Solver_env.Sys_var.Bindings.union
          solver_env_from_context.Dune_pkg.Solver_env.sys
          sys_bindings_from_current_system
      with
      | Ok solver_env -> solver_env
      | Error
          (`Var_in_both_with_different_values
            (var, value_from_context, value_from_system)) ->
        User_error.raise
          [ Pp.textf "Can't create solver environment for context %s"
              (String.maybe_quoted
              @@ Dune_engine.Context_name.to_string context_name)
          ; Pp.text
              "This can happen if --use-env-from-current-system is passed and \
               the build context specifies environment variables which differ \
               from those of the current system."
          ; Pp.textf "Conflicting values for system environment variable: %s"
              (String.maybe_quoted @@ Dune_pkg.Solver_env.Sys_var.to_string var)
          ; Pp.textf "The value inferred from the current system is: %s"
              (String.maybe_quoted value_from_system)
          ; Pp.textf "The value configured in the build context (%s) is: %s"
              (String.maybe_quoted
              @@ Dune_engine.Context_name.to_string context_name)
              (String.maybe_quoted value_from_context)
          ]
    in
    { solver_env_from_context with Dune_pkg.Solver_env.sys }

  (* Set the opam-version variable, raising a user error if it's already set. *)
  let add_opam_version_to_solver_env context_name
      (solver_env : Dune_pkg.Solver_env.t) =
    match
      Dune_pkg.Solver_env.Sys_var.Bindings.get solver_env.sys `Opam_version
    with
    | Some _ ->
      User_error.raise
        [ Pp.textf
            "Context %s would override solver variable %s. This variable may \
             not be overriden."
            (String.maybe_quoted
            @@ Dune_engine.Context_name.to_string context_name)
            (Dune_pkg.Solver_env.Sys_var.to_string `Opam_version)
        ]
    | None ->
      { solver_env with
        sys =
          Dune_pkg.Solver_env.Sys_var.Bindings.set solver_env.sys `Opam_version
            (OpamVersion.to_string OpamVersion.current)
      }

  let context_term =
    Arg.(
      value
      & opt (some Arg.context_name) None
      & info [ "context" ] ~docv:"CONTEXT"
          ~doc:
            "Generate the lockdir associated with this context (the default \
             context will be used if this is omitted)")

  let term =
    let+ (common : Common.t) = Common.term
    and+ opam_repository_path = Opam_repository_path.term
    and+ opam_repository_url = Opam_repository_url.term
    and+ context_name = context_term
    and+ all_contexts =
      Arg.(
        value & flag
        & info [ "all-contexts" ] ~doc:"Generate the lockdir for all contexts")
    and+ version_preference = Version_preference.term
    and+ use_env_from_current_system =
      Arg.(
        value & flag
        & info
            [ "use-env-from-current-system" ]
            ~doc:
              "Set opam system environment variables based on the current \
               system when solving dependencies. This will restrict packages \
               to just those which can be installed on the current system. \
               Note that this will mean that the generated lockdir may not be \
               compatible with other systems. If the build context(s) specify \
               system environment variables then the solver will use the union \
               of the environment variables infered from the current system \
               and the environment variables set in the build context(s). If \
               the current system and the build context(s) disagree on the \
               value of an environment variable then an error is raised.")
    and+ just_print_solver_env =
      Arg.(
        value & flag
        & info
            [ "just-print-solver-env" ]
            ~doc:
              "Print an s-expression describing the environment that would be \
               used to solve dependencies and then exit without attempting to \
               solve the dependencies or generate the lockfile. Intended to be \
               used to debug situations where no solution can be found to a \
               project's dependencies.")
    in
    let common = Common.forbid_builds common in
    let config = Common.init common in
    Scheduler.go ~common ~config @@ fun () ->
    let open Fiber.O in
    let* per_context =
      Per_context.choose ~context_name_arg:context_name
        ~all_contexts_arg:all_contexts
        ~version_preference_arg:version_preference
    in
    let* sys_bindings_from_current_system =
      if use_env_from_current_system then
        Dune_pkg.Sys_poll.sys_bindings ~path:(Env_path.path Stdune.Env.initial)
      else Fiber.return Dune_pkg.Solver_env.Sys_var.Bindings.empty
    in
    if just_print_solver_env then
      let+ () = Fiber.return () in
      List.iter per_context
        ~f:(fun
             { Per_context.solver_env = solver_env_from_context
             ; context_common = { name = context_name; _ }
             ; _
             }
           ->
          let solver_env =
            merge_current_system_bindings_into_solver_env_from_context
              ~context_name ~solver_env_from_context
              ~sys_bindings_from_current_system
            |> add_opam_version_to_solver_env context_name
          in
          Console.print
            [ Pp.textf "Solver environment for context %s:"
                (String.maybe_quoted
                @@ Dune_engine.Context_name.to_string context_name)
            ; Pp.verbatim
                (Dune_sexp.to_string @@ Dune_pkg.Solver_env.encode solver_env)
            ])
    else (
      Per_context.check_for_dup_lock_dir_paths per_context;
      (* a list of thunks that will perform all the file IO side
         effects after performing validation so that if materializing any
         lockdir would fail then no side effect takes place. *)
      (let+ opam_file_map =
         let+ dune_package_map =
           let+ source_dir = Memo.run (Source_tree.root ()) in
           let project = Source_tree.Dir.project source_dir in
           Dune_project.packages project
         in
         opam_file_map_of_dune_package_map dune_package_map
       and+ repo =
         let+ opam_repo_dir =
           match opam_repository_path with
           | Some path -> Fiber.return path
           | None -> (
             let repo =
               Option.map ~f:Opam_repository.of_url opam_repository_url
               |> Option.value ~default:Opam_repository.default
             in
             let+ opam_repository = Opam_repository.path repo in
             match opam_repository with
             | Ok path -> path
             | Error _ -> failwith "TODO")
         in
         Dune_pkg.Opam_repo.of_opam_repo_dir_path opam_repo_dir
       in
       List.map per_context
         ~f:(fun
              { Per_context.lock_dir_path
              ; version_preference
              ; solver_env = solver_env_from_context
              ; context_common = { name = context_name; _ }
              }
            ->
           let solver_env =
             merge_current_system_bindings_into_solver_env_from_context
               ~context_name ~solver_env_from_context
               ~sys_bindings_from_current_system
             |> add_opam_version_to_solver_env context_name
           in
           match
             Dune_pkg.Opam_solver.solve_lock_dir solver_env version_preference
               repo ~local_packages:opam_file_map
           with
           | Error (`Diagnostic_message message) -> Error (context_name, message)
           | Ok (summary, lock_dir) ->
             let summary_message =
               Dune_pkg.Opam_solver.Summary.selected_packages_message summary
                 ~lock_dir_path
               |> User_message.pp
             in
             Ok
               ( Lock_dir.Write_disk.prepare ~lock_dir_path lock_dir
               , summary_message ))
       |> Result.List.all)
      >>| function
      | Error (context_name, message) ->
        User_error.raise
          [ Pp.textf "Unable to solve dependencies in build context: %s"
              (Dune_engine.Context_name.to_string context_name
              |> String.maybe_quoted)
          ; message
          ]
      | Ok write_disks_with_summaries ->
        let write_disk_list, summary_pps =
          List.split write_disks_with_summaries
        in
        Dune_console.print summary_pps;
        (* All the file IO side effects happen here: *)
        List.iter write_disk_list ~f:Lock_dir.Write_disk.commit)

  let info =
    let doc = "Create a lockfile" in
    Cmd.info "lock" ~doc

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

let group = Cmd.group info [ Lock.command ]
