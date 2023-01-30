open Stdune
open Import

let doc =
  "Execute a command in a similar environment as if installation was performed."

let man =
  [ `S "DESCRIPTION"
  ; `P
      {|$(b,dune exec -- COMMAND) should behave in the same way as if you
          do:|}
  ; `Pre "  \\$ dune install\n  \\$ COMMAND"
  ; `P
      {|In particular if you run $(b,dune exec ocaml), you will have
          access to the libraries defined in the workspace using your usual
          directives ($(b,#require) for instance)|}
  ; `P
      {|When a leading / is present in the command (absolute path), then the
          path is interpreted as an absolute path|}
  ; `P
      {|When a / is present at any other position (relative path), then the
          path is interpreted as relative to the build context + current
          working directory (or the value of $(b,--root) when ran outside of
          the project root)|}
  ; `Blocks Common.help_secs
  ; Common.examples
      [ ("Run the executable named `my_exec'", "dune exec my_exec")
      ; ( "Run the executable defined in `foo.ml' with the argument `arg'"
        , "dune exec -- ./foo.exe arg" )
      ]
  ]

let info = Cmd.info "exec" ~doc ~man

module Command_to_exec = struct
  (* A command to execute, which knows how to (re)build the program and then
     run it with some arguments in an enivorment *)

  type t =
    { get_path_and_build_if_necessary :
        unit -> (Path.t, [ `Already_reported ]) result Fiber.t
    ; args : string list
    ; env : Env.t
    }

  (* Helper function to spawn a new process running a command in an
     environment, returning the new process' pid *)
  let spawn_process path ~args ~env =
    let path = Path.to_string path in
    let env = Env.to_unix env |> Spawn.Env.of_list in
    let argv = path :: args in
    let pid = Spawn.spawn ~prog:path ~env ~argv () in
    Pid.of_int pid

  (* Run the command, first (re)building the program which the command is
     invoking *)
  let build_and_run_in_child_process
      { get_path_and_build_if_necessary; args; env } =
    get_path_and_build_if_necessary ()
    |> Fiber.map ~f:(Result.map ~f:(spawn_process ~args ~env))
end

module Watch = struct
  (* When running `dune exec` in watch mode, this will keep track of the pid of
     the process created to run the program in the previous iteration so that
     it can be killed (for long running programs, e.g. servers) and restarted
     when its source is changed. *)

  type state = { currently_running_pid : Pid.t option ref }

  let init_state () = { currently_running_pid = ref None }

  let kill_process pid =
    let pid_int = Pid.to_int pid in
    (* TODO This logic should exist in one place. Currently it's here and in
       the scheduler *)
    let signal = if Sys.win32 then Sys.sigkill else Sys.sigterm in
    (* FIXME Since we're reaping in a different thread, this can technically
       cause pid reuse *)
    Unix.kill pid_int signal;
    let do_wait () =
      Scheduler.wait_for_process ~timeout:1. pid
      |> Fiber.map ~f:(fun (_ : Proc.Process_info.t) -> ())
    in
    let on_error (e : Exn_with_backtrace.t) =
      (* Ignore [Build_cancelled] exception we expect the build to be cancelled
         if the source is changed during compilation. *)
      match e.exn with
      | Memo.Non_reproducible Scheduler.Run.Build_cancelled -> Fiber.return ()
      | _ -> Exn_with_backtrace.reraise e
    in
    Fiber.map_reduce_errors (module Monoid.Unit) ~on_error do_wait
    |> Fiber.map ~f:(function Ok () | Error () -> ())

  let kill_currently_running_process { currently_running_pid } =
    match !currently_running_pid with
    | None -> Fiber.return ()
    | Some pid ->
      currently_running_pid := None;
      kill_process pid

  (* Kills the currently running process, then runs the given command after
     (re)building the program which it will invoke *)
  let run state ~command_to_exec =
    let open Fiber.O in
    let* () = Fiber.return () in
    let* () = kill_currently_running_process state in
    Command_to_exec.build_and_run_in_child_process command_to_exec
    >>| Result.map ~f:(fun pid -> state.currently_running_pid := Some pid)

  let loop ~command_to_exec =
    let state = init_state () in
    Scheduler.Run.poll (run state ~command_to_exec)
end

let build_prog ~no_rebuild ~prog p =
  if no_rebuild then
    if Path.exists p then Memo.return p
    else
      User_error.raise
        [ Pp.textf
            "Program %S isn't built yet. You need to build it first or remove \
             the --no-build option."
            prog
        ]
  else
    let open Memo.O in
    let+ () = Build_system.build_file p in
    p

let not_found ~dir ~prog =
  let open Memo.O in
  let+ hints =
    (* Good candidates for the "./x.exe" instead of "x.exe" error are
        executables present in the current directory. Note: we do not
        check directory targets here; even if they do indeed include a
        matching executable, they would be located in a subdirectory of
        [dir], so it's unclear if that's what the user wanted. *)
    let+ candidates =
      Build_system.files_of ~dir:(Path.build dir)
      >>| Path.Set.to_list
      >>| List.filter ~f:(fun p -> Path.extension p = ".exe")
      >>| List.map ~f:(fun p -> "./" ^ Path.basename p)
    in
    User_message.did_you_mean prog ~candidates
  in
  User_error.raise ~hints [ Pp.textf "Program %S not found!" prog ]

let get_path_and_build_if_necessary sctx ~no_rebuild ~dir ~prog =
  let open Memo.O in
  match Filename.analyze_program_name prog with
  | In_path -> (
    Super_context.resolve_program sctx ~dir ~loc:None prog >>= function
    | Error (_ : Action.Prog.Not_found.t) -> not_found ~dir ~prog
    | Ok p -> build_prog ~no_rebuild ~prog p)
  | Relative_to_current_dir -> (
    let path = Path.relative_to_source_in_build_or_external ~dir prog in
    (Build_system.file_exists path >>= function
     | true -> Memo.return (Some path)
     | false -> (
       if not (Filename.check_suffix prog ".exe") then Memo.return None
       else
         let path = Path.extend_basename path ~suffix:".exe" in
         Build_system.file_exists path >>| function
         | true -> Some path
         | false -> None))
    >>= function
    | Some path -> build_prog ~no_rebuild ~prog path
    | None -> not_found ~dir ~prog)
  | Absolute -> (
    match
      let prog = Path.of_string prog in
      if Path.exists prog then Some prog
      else if not Sys.win32 then None
      else
        let prog = Path.extend_basename prog ~suffix:Bin.exe in
        Option.some_if (Path.exists prog) prog
    with
    | Some prog -> Memo.return prog
    | None -> not_found ~dir ~prog)

module Exec_context = struct
  type t =
    { common : Common.t
    ; config : Dune_config.t
    ; args : string list
    ; env : Env.t Fiber.t
    ; get_path_and_build_if_necessary : (unit -> Path.t Memo.t) Fiber.t
    }

  let init ~common ~context ~no_rebuild ~prog ~args =
    (* The initialization of some fields is deferred until the fiber scheduler
       has been started. *)
    let config = Common.init common in
    let sctx =
      let open Fiber.O in
      let* setup = Import.Main.setup () in
      let+ setup = Memo.run setup in
      Import.Main.find_scontext_exn setup ~name:context
    in
    let dir =
      Fiber.map sctx ~f:(fun sctx ->
          let context = Dune_rules.Super_context.context sctx in
          Path.Build.relative context.build_dir (Common.prefix_target common ""))
    in
    let env = Fiber.map sctx ~f:Super_context.context_env in
    let get_path_and_build_if_necessary =
      let open Fiber.O in
      let* sctx = sctx in
      let+ dir = dir in
      fun () -> get_path_and_build_if_necessary sctx ~no_rebuild ~dir ~prog
    in
    { common; config; env; args; get_path_and_build_if_necessary }

  let run_once { common; config; env; args; get_path_and_build_if_necessary; _ }
      =
    Scheduler.go ~common ~config @@ fun () ->
    let open Fiber.O in
    let* get_path_and_build_if_necessary = get_path_and_build_if_necessary in
    let* env = env in
    let+ path = Build_system.run_exn get_path_and_build_if_necessary in
    let prog = Path.to_string path in
    let argv = prog :: args in
    restore_cwd_and_execve common prog argv env

  let run_eager_watch
      { common; config; env; args; get_path_and_build_if_necessary; _ } =
    Scheduler.go_with_rpc_server_and_console_status_reporting ~common ~config
    @@ fun () ->
    let open Fiber.O in
    let* get_path_and_build_if_necessary = get_path_and_build_if_necessary in
    let* env = env in
    let command_to_exec =
      { Command_to_exec.get_path_and_build_if_necessary =
          (fun () ->
            (* TODO we should release the dune lock. But we aren't doing it
               because we don't unload the database files we've marshalled.
            *)
            Build_system.run get_path_and_build_if_necessary)
      ; args
      ; env
      }
    in
    Watch.loop ~command_to_exec
end

let term =
  let+ common = Common.term
  and+ context =
    Common.context_arg ~doc:{|Run the command in this build context.|}
  and+ prog =
    Arg.(required & pos 0 (some string) None (Arg.info [] ~docv:"PROG"))
  and+ no_rebuild =
    Arg.(
      value & flag
      & info [ "no-build" ] ~doc:"don't rebuild target before executing")
  and+ args = Arg.(value & pos_right 0 string [] (Arg.info [] ~docv:"ARGS")) in
  (* TODO we should make sure to finalize the current backend before exiting dune.
     For watch mode, we should finalize the backend and then restart it in between
     runs. *)
  let exec_context =
    Exec_context.init ~common ~context ~no_rebuild ~prog ~args
  in
  match Common.watch common with
  | Yes Passive ->
    User_error.raise [ Pp.textf "passive watch mode is unsupported by exec" ]
  | Yes Eager -> Exec_context.run_eager_watch exec_context
  | No -> Exec_context.run_once exec_context

let command = Cmd.v info term
