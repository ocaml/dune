open Import

let doc = "Open a shell in the prepared environment of a rule action."

let man =
  [ `S "DESCRIPTION"
  ; `P
      {|$(b,dune shell TARGET) resolves one concrete target to its rule, builds the
         rule's dependencies, and opens a shell in the exact execution location and
         sandbox mode selected for its action.|}
  ; `P
      {|A command after $(b,--) is executed instead of an interactive shell. The
         command receives the same prepared environment and working directory.|}
  ; `Blocks Common.help_secs
  ]
;;

let info = Cmd.info "shell" ~doc ~man

let resolved_program ~env ~dir program =
  match Filename.analyze_program_name program with
  | Absolute -> Path.of_string program
  | Relative_to_current_dir -> Path.relative dir program
  | In_path ->
    let path =
      match Env.get env Env_path.var with
      | None -> []
      | Some value ->
        Bin.parse value
        |> List.map ~f:(fun entry ->
          if String.is_empty entry
          then dir
          else if Filename.is_relative entry
          then Path.relative dir entry
          else Path.of_string entry)
    in
    (match Bin.which ~path program with
     | Some path -> path
     | None ->
       User_error.raise
         [ Pp.textf "Program %S was not found in the prepared PATH." program ])
;;

module Action_file = struct
  let parse path =
    let contents = Io.read_file path in
    let ast =
      Dune_lang.Parser.parse_string contents ~fname:(Path.to_string path) ~mode:Single
    in
    Dune_lang.Decoder.parse Dune_rules.Action_for_shell.Replay.decode Univ_map.empty ast
  ;;

  let expand parsed ~session_root ~dir ~loc =
    let normalize_absolute path =
      match String.drop_prefix path ~prefix:"/" with
      | None ->
        Code_error.raise
          "dune shell replay expected an absolute POSIX path"
          [ "path", Dyn.string path ]
      | Some path ->
        Path.Local.of_string path
        |> Path.Local.to_string
        |> Path.External.relative Path.External.root
    in
    let resolve ~dir path =
      let absolute =
        if Filename.is_relative path
        then
          Path.External.relative
            (Path.External.of_string (Path.to_absolute_filename dir))
            path
        else Path.External.of_string path
      in
      normalize_absolute (Path.External.to_string absolute) |> Path.external_
    in
    let ensure_session_build_path path =
      match Path.as_in_build_dir path with
      | None -> path
      | Some _ ->
        if Path.equal path session_root || Path.is_descendant path ~of_:session_root
        then path
        else
          User_error.raise
            ~loc
            [ Pp.textf
                "Edited action path %s escapes the dune shell session."
                (Path.to_string_maybe_quoted path)
            ]
    in
    let path ~dir path =
      let resolved = resolve ~dir path in
      let localized = Path.Expert.try_localize_external resolved in
      match Path.as_in_build_dir localized with
      | Some _ -> ensure_session_build_path localized
      | None -> resolved
    in
    let target ~dir target =
      let path = path ~dir target in
      match Path.as_in_build_dir path with
      | Some target -> target
      | None ->
        User_error.raise
          ~loc
          [ Pp.textf
              "Edited action target %s is outside the dune shell session."
              (Path.to_string_maybe_quoted path)
          ]
    in
    let program ~dir = function
      | Action.For_shell.Program.Resolved program -> Ok (path ~dir program)
      | Unresolved { context; program; hint } ->
        Error (Action.Prog.Not_found.create ?hint ~context ~program ~loc:(Some loc) ())
    in
    let rec loop (action : Action.For_shell.t) ~dir =
      match action with
      | Run { prog; args; can_run_in_action_runner = _ } ->
        Action.Run { prog = program ~dir prog; args; can_run_in_action_runner = false }
      | With_accepted_exit_codes (codes, action) ->
        Action.With_accepted_exit_codes (codes, loop action ~dir)
      | Chdir (chdir, action) ->
        let chdir = path ~dir chdir in
        if
          not (Path.equal chdir session_root || Path.is_descendant chdir ~of_:session_root)
        then
          User_error.raise
            ~loc
            [ Pp.text "Edited action chdir escapes the dune shell session." ];
        Action.Chdir (chdir, loop action ~dir:chdir)
      | Setenv (var, value, action) -> Action.Setenv (var, value, loop action ~dir)
      | Redirect_out (outputs, target_, perm, action) ->
        Action.Redirect_out (outputs, target ~dir target_, perm, loop action ~dir)
      | Redirect_in (inputs, path_, action) ->
        Action.Redirect_in (inputs, path ~dir path_, loop action ~dir)
      | Ignore (outputs, action) -> Action.Ignore (outputs, loop action ~dir)
      | Progn actions -> Action.Progn (List.map actions ~f:(loop ~dir))
      | Concurrent _ ->
        User_error.raise
          ~loc
          [ Pp.text "Edited concurrent actions cannot be replayed."
          ; Pp.text "Replay does not define how to combine concurrent raw exit statuses."
          ]
      | Echo strings -> Action.Echo strings
      | Cat paths -> Action.Cat (List.map paths ~f:(path ~dir))
      | Copy (source, target_) -> Action.Copy (path ~dir source, target ~dir target_)
      | Symlink (source, target_) ->
        let target = target ~dir target_ in
        let source_dir = Path.build (Path.Build.parent_exn target) in
        let source =
          resolve ~dir:source_dir source |> Path.Expert.try_localize_external
        in
        Action.Symlink (source, target)
      | Hardlink (source, target_) ->
        Action.Hardlink (path ~dir source, target ~dir target_)
      | System command -> Action.System command
      | Bash { script; can_run_in_action_runner = _ } ->
        Action.Bash { script; can_run_in_action_runner = false }
      | Write_file (target_, perm, contents) ->
        Action.Write_file (target ~dir target_, perm, contents)
      | Rename (source, target_) -> Action.Rename (target ~dir source, target ~dir target_)
      | Remove_tree target_ -> Action.Remove_tree (target ~dir target_)
      | Mkdir target_ -> Action.Mkdir (target ~dir target_)
      | Pipe (outputs, actions) -> Action.Pipe (outputs, List.map actions ~f:(loop ~dir))
      | Diff { optional; mode; file1; file2; directory_diffs } ->
        Action.Diff
          { optional
          ; mode
          ; file1 = path ~dir file1
          ; file2 = target ~dir file2
          ; directory_diffs
          }
      | Extension _ ->
        User_error.raise ~loc [ Pp.text "Edited extension actions cannot be replayed." ]
    in
    loop parsed ~dir
  ;;
end

module Metadata_codec = struct
  module Conv = Dune_rpc.Conv

  let strings = Conv.(list string)

  let env =
    Conv.iso strings (fun entries -> Env.of_unix (Array.of_list entries)) Env.to_unix
  ;;

  let optional_string = Conv.(option string)

  let targets =
    let to_ (file_paths, dir_paths) =
      Result.try_with (fun () ->
        let paths paths =
          List.map paths ~f:Path.Build.of_string |> Path.Build.Set.of_list
        in
        match
          Targets.create ~files:(paths file_paths) ~dirs:(paths dir_paths)
          |> Targets.validate
        with
        | Valid targets -> targets
        | No_targets
        | Inconsistent_parent_dir
        | File_and_directory_target_with_the_same_name _ ->
          Code_error.raise "invalid dune shell targets" [])
    in
    let from targets =
      let payload path = Path.Build.local path |> Path.Local.to_string in
      let files = ref [] in
      let dirs = ref [] in
      Targets.Validated.iter
        targets
        ~file:(fun path -> files := payload path :: !files)
        ~dir:(fun path -> dirs := payload path :: !dirs);
      List.rev !files, List.rev !dirs
    in
    Conv.iso_result Conv.(pair strings strings) to_ from
  ;;

  let session =
    let open Conv in
    pair
      (triple string string string)
      (pair (triple string string string) (pair (triple env env optional_string) targets))
  ;;

  let write path codec value =
    Conv.to_sexp codec value |> Csexp.to_string |> Io.write_file path
  ;;

  let read path codec ~description =
    let invalid details =
      User_error.raise
        ~loc:(Loc.in_file path)
        (Pp.textf "Invalid %s in dune shell metadata." description :: details)
    in
    match Csexp.parse_string (Io.read_file path) with
    | Error _ -> invalid []
    | Ok sexp ->
      (match Conv.of_sexp codec ~version:(0, 0) sexp with
       | Ok value -> value
       | Error error -> invalid [ Pp.text (Dyn.to_string (Conv.dyn_of_error error)) ])
  ;;
end

let dune_binary () = Util.dune_executable () |> Path.to_absolute_filename
let write_exact metadata name value = Io.write_file (Path.relative metadata name) value
let build_path_payload path = Path.Build.local path |> Path.Local.to_string

let write_runner metadata_dir =
  let quote = String.quote_for_shell in
  let command =
    [ dune_binary (); "internal"; "shell-replay"; Path.to_absolute_filename metadata_dir ]
    |> List.map ~f:quote
    |> String.concat ~sep:" "
  in
  let contents = sprintf "#!/bin/sh\nexec %s\n" command in
  List.iter [ "dune-run"; "run" ] ~f:(fun name ->
    Io.write_file
      (Path.relative metadata_dir name)
      contents
      ~perm:Permissions.Mode.executable_file)
;;

let mode_note = function
  | None ->
    "sandbox mode none: this is the real _build directory; commands and run write there \
     directly."
  | Some Sandbox_mode.Copy ->
    "sandbox mode copy: this is the action's canonical digest sandbox, destroyed when \
     the session ends."
  | Some Sandbox_mode.Symlink ->
    "sandbox mode symlink: dependencies may point at shared build artifacts."
  | Some Sandbox_mode.Hardlink ->
    "sandbox mode hardlink: writes may affect shared build artifacts."
  | Some Sandbox_mode.Patch_back_source_tree ->
    Code_error.raise "patch-back dune shell should have been rejected" []
;;

(* The session helper commands are bash functions sourced into the interactive
   shell. They read the files written under [$DUNE_SHELL]. The help screen also
   shows the consequential differences from a real action execution. *)
let write_shell_init metadata ~mode ~has_direct_process_metadata =
  let init = Path.relative metadata "init.bash" in
  let command_fn, command_help =
    if has_direct_process_metadata
    then
      ( {|show_command () {
  cat "$DUNE_SHELL/command"
}

|}
      , "  show_command   print the exact direct program, args, and env\n" )
    else "", ""
  in
  let notes =
    String.concat
      ~sep:"\n"
      [ mode_note mode
      ; "This is not an isolation boundary; commands can modify your source tree."
      ; "run clears the action's declared targets and replays only the action; it does \
         not rebuild dependencies or pick up edits made after entry."
      ]
  in
  let contents =
    sprintf
      {|# Loaded into the dune shell session. Passing --rcfile skips the user's
# ~/.bashrc, so load it here to keep their normal environment, then define
# the session helper commands on top and show the help screen.
if [ -f "$HOME/.bashrc" ]; then . "$HOME/.bashrc"; fi

run () {
  "$DUNE_SHELL/dune-run" "$@"
}

show_action () {
  cat "$DUNE_SHELL/action.sexp"
}

%shelp () {
  cat <<'HELP'
dune shell session commands:
  run            replay the prepared action
  show_action    print or edit the expanded action ($DUNE_SHELL/action.sexp)
%s  help           show this message

%s
HELP
}

help
|}
      command_fn
      command_help
      notes
  in
  Io.write_file init contents;
  init
;;

type direct_process =
  { program : Path.t
  ; args : string list
  ; dir : Path.t
  ; env : Env.t
  }

let prepared_temp_dir (shell : Build_system.Rule_shell.t) =
  match Env.get shell.replay_env Env.Var.temp_dir with
  | Some temp_dir -> temp_dir
  | None -> Code_error.raise "prepared dune shell environment has no temp dir" []
;;

let direct_process (shell : Build_system.Rule_shell.t) =
  let rec loop action ~dir ~env =
    match (action : Action.t) with
    | Run { prog = Ok program; args; can_run_in_action_runner = _ } ->
      Some { program; args = Appendable_list.to_list args; dir; env }
    | Run { prog = Error _; _ } -> None
    | Bash { script; can_run_in_action_runner = _ } ->
      Option.map
        (Bin.which ~path:(Env_path.path Env.initial) "bash")
        ~f:(fun program ->
          { program; args = [ "-e"; "-u"; "-o"; "pipefail"; "-c"; script ]; dir; env })
    | Chdir (dir, action) -> loop action ~dir ~env
    | Setenv (var, value, action) -> loop action ~dir ~env:(Env.add env ~var ~value)
    | With_accepted_exit_codes (_, action)
    | Redirect_out (_, _, _, action)
    | Redirect_in (_, _, action)
    | Ignore (_, action) -> loop action ~dir ~env
    | Progn [ action ] -> loop action ~dir ~env
    | _ -> None
  in
  Option.map (loop shell.action ~dir:shell.dir ~env:shell.replay_env) ~f:(fun process ->
    let temp_dir = prepared_temp_dir shell in
    let env = Env.add process.env ~var:Env.Var.temp_dir ~value:temp_dir in
    { process with env })
;;

let write_direct_process_metadata shell metadata =
  match direct_process shell with
  | None -> false
  | Some { program; args; dir; env } ->
    let argv = Path.to_absolute_filename program :: args in
    Metadata_codec.write
      (Path.relative metadata "command.argv.csexp")
      Metadata_codec.strings
      argv;
    Metadata_codec.write
      (Path.relative metadata "command.env.csexp")
      Metadata_codec.env
      env;
    write_exact metadata "command.cwd" (Path.to_absolute_filename dir);
    Io.write_lines (Path.relative metadata "command.env") (Env.to_unix env);
    List.map argv ~f:String.quote_for_shell
    |> String.concat ~sep:" "
    |> write_exact metadata "command";
    true
;;

let write_metadata (shell : Build_system.Rule_shell.t) ~metadata =
  let metadata_absolute = Path.to_absolute_filename metadata in
  let env =
    shell.shell_env
    |> Env.add ~var:"DUNE_SHELL" ~value:metadata_absolute
    |> Env_path.cons ~dir:metadata
  in
  let action_contents =
    Action.for_shell_replay shell.action ~dir:shell.dir
    |> Dune_rules.Action_for_shell.Replay.encode
    |> Dune_lang.pp
    |> Format.asprintf "%a\n" Pp.to_fmt
  in
  Io.write_file (Path.relative metadata "action.sexp") action_contents;
  Io.write_file
    (Path.relative metadata "rule-digest")
    (Digest.to_string shell.rule_digest ^ "\n");
  Io.write_file
    (Path.relative metadata "sandbox")
    ((match shell.sandbox_dir with
      | None -> "none"
      | Some dir -> Path.to_absolute_filename (Path.build dir))
     ^ "\n");
  Io.write_file
    (Path.relative metadata "sandbox-mode")
    (Sandbox_mode.to_string shell.sandbox_mode ^ "\n");
  let cwd = Path.as_in_build_dir_exn shell.dir in
  let session_root = Option.value shell.sandbox_dir ~default:Path.Build.root in
  let temp_dir = prepared_temp_dir shell in
  Metadata_codec.write
    (Path.relative metadata "session.csexp")
    Metadata_codec.session
    ( ( Path.to_absolute_filename Path.root
      , Path.to_absolute_filename Path.build_dir
      , build_path_payload cwd )
    , ( (build_path_payload session_root, temp_dir, dune_binary ())
      , ((Env.initial, shell.replay_env, !Clflags.diff_command), shell.targets) ) );
  let has_direct_process_metadata = write_direct_process_metadata shell metadata in
  write_runner metadata;
  env, has_direct_process_metadata
;;

let resolve_rule ~target common =
  let open Memo.O in
  let* setup = Util.setup () in
  let* requests =
    Target.resolve_targets_exn (Common.root common) setup [ target ]
    |> Action_builder.evaluate_and_collect_facts
    >>| fst
  in
  let path =
    match requests with
    | [ Target.Request.File path ] -> path
    | [ Alias _ ] ->
      User_error.raise
        [ Pp.text "dune shell requires a concrete file or directory target."
        ; Pp.text "Aliases select multiple actions and are not supported yet."
        ]
    | [] | _ :: _ :: _ ->
      User_error.raise [ Pp.text "dune shell requires exactly one concrete target." ]
  in
  let+ rule =
    Load_rules.get_rule path
    >>| function
    | Some rule -> rule
    | None ->
      User_error.raise
        [ Pp.textf "No rule produces %s." (Path.to_string_maybe_quoted path) ]
  in
  path, rule
;;

let locate_bash () =
  match Bin.which ~path:(Env_path.path Env.initial) "bash" with
  | Some bash -> bash
  | None ->
    User_error.raise
      [ Pp.text "dune shell requires bash to open an interactive session."
      ; Pp.text "Install bash, or use `dune shell TARGET -- COMMAND` to run a command."
      ]
;;

let run_in_rule_shell ~target ~command common =
  Build.build_memo_exn (fun () ->
    let open Memo.O in
    let* _path, rule = resolve_rule ~target common in
    Build_system.Rule_shell.with_ rule ~f:(fun shell ->
      let metadata =
        Temp.create
          ~perms:(Permissions.Mode.create ~user:Permissions.(read + write + execute) ())
          Dir
          ~prefix:"dune-shell"
          ~suffix:"session"
      in
      Fiber.finalize
        ~finally:(fun () ->
          Temp.destroy Dir metadata;
          Fiber.return ())
        (fun () ->
           let env, has_direct_process_metadata = write_metadata shell ~metadata in
           (* Startup prints nothing from Dune itself. In an interactive session
              the sourced [init.bash] shows the help screen; in command mode the
              output belongs entirely to the user's command. *)
           let program, args =
             match command with
             | [] ->
               let init =
                 write_shell_init
                   metadata
                   ~mode:shell.sandbox_mode
                   ~has_direct_process_metadata
                 |> Path.to_absolute_filename
               in
               locate_bash (), [ "--rcfile"; init ]
             | program :: args -> resolved_program ~env ~dir:shell.dir program, args
           in
           Console.Status_line.clear ();
           Dune_engine.Process.run_inherit_std_in_out_raw ~dir:shell.dir ~env program args
           |> Fiber.map ~f:Dune_engine.Process.Failure_mode.exit_code_of_raw_status)))
;;

let term =
  let+ builder = Common.Builder.term
  and+ target = Arg.(required & pos 0 (some dep) None & info [] ~docv:"TARGET" ~doc:None)
  and+ command =
    Arg.(value & pos_right 0 string [] & info [] ~docv:"COMMAND" ~doc:None)
  in
  if Sys.win32
  then
    User_error.raise
      [ Pp.text "dune shell does not support native Windows yet."
      ; Pp.text "The replay runner currently requires a POSIX shell."
      ];
  let common, config = Common.init_build builder in
  if Common.action_runner_requested common
  then
    User_error.raise
      [ Pp.text "dune shell does not support external action runners."
      ; Pp.text
          "The shell and replay cannot reproduce action-runner or sandbox-actions \
           process placement yet."
      ; Pp.text "Run dune shell again without those options."
      ];
  match Common.watch common with
  | Yes _ -> User_error.raise [ Pp.text "dune shell does not support watch mode." ]
  | No ->
    if Config.(get global_lock) = `Disabled
    then
      User_error.raise
        [ Pp.text "dune shell requires Dune's build-directory global lock."
        ; Pp.text "Remove DUNE_CONFIG__GLOBAL_LOCK=disabled and start the session again."
        ];
    Global_lock.lock_exn ();
    let exit_code =
      Scheduler_setup.go_without_rpc_server ~common ~config (fun () ->
        run_in_rule_shell ~target ~command common)
    in
    if exit_code <> 0
    then (
      Console.finish ();
      exit exit_code)
;;

module Internal_replay = struct
  let read_session metadata =
    Metadata_codec.read
      (Path.relative metadata "session.csexp")
      Metadata_codec.session
      ~description:"session data"
  ;;

  let initialize_paths ~workspace_root ~build_dir =
    Path.set_root (Path.External.of_string workspace_root);
    Path.Build.set_build_dir (Path.Outside_build_dir.of_string build_dir)
  ;;

  let clear_targets targets =
    Targets.Validated.iter
      targets
      ~file:(fun path -> Path.rm_rf ~chmod:true (Path.build path))
      ~dir:(fun path -> Path.rm_rf ~chmod:true (Path.build path));
    Fiber.return ()
  ;;

  let command =
    let info = Cmd.info "shell-replay" ~doc:"Replay a prepared dune shell action." in
    let term =
      let+ metadata =
        Arg.(
          required
          & pos 0 (some string) None
          & info [] ~docv:"SESSION" ~doc:(Some "dune shell metadata directory"))
      and+ environment_restored =
        Arg.(value & flag & info [ "environment-restored" ] ~doc:None)
      in
      let metadata = Path.of_filename_relative_to_initial_cwd metadata in
      let (workspace_root, build_dir, cwd), session = read_session metadata in
      let (session_root, temp_dir, dune_binary), session = session in
      let (invocation_env, action_env, diff_command), targets = session in
      if not environment_restored
      then
        Proc.restore_cwd_and_execve
          dune_binary
          [ "internal"
          ; "shell-replay"
          ; "--environment-restored"
          ; Path.to_absolute_filename metadata
          ]
          ~env:invocation_env
      else (
        initialize_paths ~workspace_root ~build_dir;
        let config = Dune_config_file.Dune_config.default in
        Dune_config.init config ~watch:false;
        Clflags.diff_command := diff_command;
        Log.init No_log_file;
        let action_path = Path.relative metadata "action.sexp" in
        let dir = Path.Build.of_string cwd |> Path.build in
        let session_root = Path.Build.of_string session_root |> Path.build in
        let temp_dir = Path.of_string temp_dir in
        let action =
          Action_file.parse action_path
          |> Action_file.expand ~session_root ~dir ~loc:(Loc.in_file action_path)
        in
        let exit_code =
          Scheduler_setup.no_build_no_rpc ~config (fun () ->
            let open Fiber.O in
            let* () = clear_targets targets in
            Dune_engine.Action_exec.replay
              { targets
              ; dir
              ; env = action_env
              ; rule_loc = Loc.in_file action_path
              ; action
              ; temp_dir
              })
        in
        if exit_code <> 0
        then (
          Console.finish ();
          exit exit_code))
    in
    Cmd.v info term
  ;;
end

let command = Cmd.v info term
