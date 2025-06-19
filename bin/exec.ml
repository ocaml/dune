open Import

let doc = "Execute a command in a similar environment as if installation was performed."

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
      [ "Run the executable named `my_exec'", "dune exec my_exec"
      ; ( "Run the executable defined in `foo.ml' with the argument `arg'"
        , "dune exec -- ./foo.exe arg" )
      ]
  ]
;;

let info = Cmd.info "exec" ~doc ~man

module Cmd_arg = struct
  type t =
    | Expandable of Dune_lang.String_with_vars.t * string
    | Terminal of string

  let parse s =
    match Arg.conv_parser Arg.dep s with
    | Ok (File sw) when Dune_lang.String_with_vars.has_pforms sw -> Expandable (sw, s)
    | _ -> Terminal s
  ;;

  let pp pps = function
    | Expandable (_, s) -> Format.fprintf pps "%s" s
    | Terminal s -> Format.fprintf pps "%s" s
  ;;

  let expand t ~root ~sctx =
    let open Memo.O in
    match t with
    | Terminal s -> Memo.return s
    | Expandable (sw, _) ->
      let+ path, _ =
        Target.expand_path_from_root root sctx sw
        |> Action_builder.evaluate_and_collect_facts
      in
      let context = Dune_rules.Super_context.context sctx in
      (* TODO Why are we stringifying this path? *)
      Path.to_string (Path.build (Path.Build.relative (Context.build_dir context) path))
  ;;

  let conv = Arg.conv ((fun s -> Ok (parse s)), pp)
end

let not_found ~hints ~prog =
  User_error.raise
    ~hints
    [ Pp.concat
        ~sep:Pp.space
        [ Pp.text "Program"; User_message.command prog; Pp.text "not found!" ]
    ]
;;

let not_found_with_suggestions ~dir ~prog =
  let open Memo.O in
  let+ hints =
    (* Good candidates for the "./x.exe" instead of "x.exe" error are
       executables present in the current directory. Note: we do not
       check directory targets here; even if they do indeed include a
       matching executable, they would be located in a subdirectory of
       [dir], so it's unclear if that's what the user wanted. *)
    let+ candidates =
      let+ filename_set = Build_system.files_of ~dir:(Path.build dir) in
      Filename_set.filenames filename_set
      |> Filename.Set.to_list
      |> List.filter ~f:(fun filename -> Filename.extension filename = ".exe")
      |> List.map ~f:(fun filename -> "./" ^ filename)
    in
    User_message.did_you_mean prog ~candidates
  in
  not_found ~hints ~prog
;;

let program_not_built_yet prog =
  User_error.raise
    [ Pp.concat
        ~sep:Pp.space
        [ Pp.text "Program"
        ; User_message.command prog
        ; Pp.text "isn't built yet. You need to build it first or remove the"
        ; User_message.command "--no-build"
        ; Pp.text "option."
        ]
    ]
;;

let build_prog ~no_rebuild ~prog p =
  if no_rebuild
  then if Path.exists p then Memo.return p else program_not_built_yet prog
  else
    let open Memo.O in
    let+ () = Build_system.build_file p in
    p
;;

let get_path_and_build_if_necessary sctx ~no_rebuild ~dir ~prog =
  let open Memo.O in
  match Filename.analyze_program_name prog with
  | In_path ->
    Super_context.resolve_program_memo sctx ~dir ~loc:None prog
    >>= (function
     | Error (_ : Action.Prog.Not_found.t) -> not_found_with_suggestions ~dir ~prog
     | Ok p -> build_prog ~no_rebuild ~prog p)
  | Relative_to_current_dir ->
    let path = Path.relative_to_source_in_build_or_external ~dir prog in
    Build_system.file_exists path
    >>= (function
     | true -> build_prog ~no_rebuild ~prog path
     | false -> not_found_with_suggestions ~dir ~prog)
  | Absolute ->
    (match
       let prog = Path.of_string prog in
       if Path.exists prog
       then Some prog
       else if not Sys.win32
       then None
       else (
         let prog = Path.extend_basename prog ~suffix:Bin.exe in
         Option.some_if (Path.exists prog) prog)
     with
     | Some prog -> Memo.return prog
     | None -> not_found_with_suggestions ~dir ~prog)
;;

let step ~setup ~prog ~args ~common ~no_rebuild ~context ~on_exit () =
  let open Memo.O in
  let* sctx = setup >>| Import.Main.find_scontext_exn ~name:context in
  let* env = Super_context.context_env sctx in
  let expand = Cmd_arg.expand ~root:(Common.root common) ~sctx in
  let* path =
    let dir =
      let context = Dune_rules.Super_context.context sctx in
      Path.Build.relative (Context.build_dir context) (Common.prefix_target common "")
    in
    let* prog = expand prog in
    get_path_and_build_if_necessary sctx ~no_rebuild ~dir ~prog
  and* args = Memo.parallel_map args ~f:expand in
  Memo.of_non_reproducible_fiber
  @@ Dune_engine.Process.run_inherit_std_in_out
       ~dir:(Path.of_string Fpath.initial_cwd)
       ~env
       path
       args
  >>| function
  | 0 -> ()
  | exit_code -> on_exit exit_code
;;

(* Similar to [get_path_and_build_if_necessary] but doesn't require the build
   system (ie. it sequences with [Fiber] rather than with [Memo]) and builds
   targets via an RPC server. Some functionality is not available but it can be
   run concurrently while a second Dune process holds the global build
   directory lock.

   Returns the absolute path to the executable. *)
let build_prog_via_rpc_if_necessary ~dir ~no_rebuild prog =
  match Filename.analyze_program_name prog with
  | In_path ->
    (* This case is reached if [dune exec] is passed the name of an
       executable (rather than a path to an executable). When dune is running
       directly, dune will try to resolve the executbale name within the public
       executables defined in the current project and its dependencies, and
       only if no executable with the given name is found will dune then
       resolve the name within the $PATH variable instead. Looking up an
       executable's name within the current project requires running the
       build system, but running the build system is not allowed while
       another dune instance holds the global build directory lock. In this
       case dune will only resolve the executable's name within $PATH.
       Because this behaviour is different from the default, print a warning
       so users are hopefully less surprised.
    *)
    User_warning.emit
      [ Pp.textf
          "As this is not the main instance of Dune it is unable to locate the \
           executable %S within this project. Dune will attempt to resolve the \
           executable's name within your PATH only."
          prog
      ];
    let path = Env_path.path Env.initial in
    (match Bin.which ~path prog with
     | None -> not_found ~hints:[] ~prog
     | Some prog_path -> Fiber.return (Path.to_absolute_filename prog_path))
  | Relative_to_current_dir ->
    let open Fiber.O in
    let path = Path.relative_to_source_in_build_or_external ~dir prog in
    let+ () =
      if no_rebuild
      then if Path.exists path then Fiber.return () else program_not_built_yet prog
      else (
        let target =
          Dune_lang.Dep_conf.File
            (Dune_lang.String_with_vars.make_text Loc.none (Path.to_string path))
        in
        Build_cmd.build_via_rpc_server ~print_on_success:false ~targets:[ target ])
    in
    Path.to_absolute_filename path
  | Absolute ->
    if Path.exists (Path.of_string prog)
    then Fiber.return prog
    else not_found ~hints:[] ~prog
;;

let exec_building_via_rpc_server ~common ~prog ~args ~no_rebuild =
  let open Fiber.O in
  let ensure_terminal v =
    match (v : Cmd_arg.t) with
    | Terminal s -> s
    | Expandable (_, raw) ->
      (* Variables cannot be expanded without running the build system. *)
      User_error.raise
        [ Pp.textf
            "The term %S contains a variable but Dune is unable to expand variables when \
             building via RPC."
            raw
        ]
  in
  let context = Common.x common |> Option.value ~default:Context_name.default in
  let dir = Context_name.build_dir context in
  let prog = ensure_terminal prog in
  let args = List.map args ~f:ensure_terminal in
  let+ prog = build_prog_via_rpc_if_necessary ~dir ~no_rebuild prog in
  restore_cwd_and_execve (Common.root common) prog args Env.initial
;;

let exec_building_directly ~common ~config ~context ~prog ~args ~no_rebuild =
  match Common.watch common with
  | Yes Passive ->
    User_error.raise [ Pp.textf "passive watch mode is unsupported by exec" ]
  | Yes Eager ->
    Scheduler.go_with_rpc_server_and_console_status_reporting ~common ~config
    @@ fun () ->
    let open Fiber.O in
    let* setup = Import.Main.setup () in
    let on_exit = Console.printf "Program exited with code [%d]" in
    Scheduler.Run.poll
    @@
    let* () = Fiber.return @@ Scheduler.maybe_clear_screen ~details_hum:[] config in
    build @@ step ~setup ~prog ~args ~common ~no_rebuild ~context ~on_exit
  | No ->
    Scheduler.go_with_rpc_server ~common ~config
    @@ fun () ->
    let open Fiber.O in
    let* setup = Import.Main.setup () in
    build_exn (fun () ->
      let open Memo.O in
      let* sctx = setup >>| Import.Main.find_scontext_exn ~name:context in
      let* env = Super_context.context_env sctx in
      let expand = Cmd_arg.expand ~root:(Common.root common) ~sctx in
      let* prog =
        let dir =
          let context = Dune_rules.Super_context.context sctx in
          Path.Build.relative (Context.build_dir context) (Common.prefix_target common "")
        in
        let* prog = expand prog in
        get_path_and_build_if_necessary sctx ~no_rebuild ~dir ~prog >>| Path.to_string
      and* args = Memo.parallel_map ~f:expand args in
      restore_cwd_and_execve (Common.root common) prog args env)
;;

let term : unit Term.t =
  let+ builder = Common.Builder.term
  and+ context = Common.context_arg ~doc:{|Run the command in this build context.|}
  and+ prog = Arg.(required & pos 0 (some Cmd_arg.conv) None (Arg.info [] ~docv:"PROG"))
  and+ no_rebuild =
    Arg.(value & flag & info [ "no-build" ] ~doc:"don't rebuild target before executing")
  and+ args = Arg.(value & pos_right 0 Cmd_arg.conv [] (Arg.info [] ~docv:"ARGS")) in
  (* TODO we should make sure to finalize the current backend before exiting dune.
     For watch mode, we should finalize the backend and then restart it in between
     runs. *)
  let common, config = Common.init builder in
  match Dune_util.Global_lock.lock ~timeout:None with
  | Error lock_held_by ->
    (match Common.watch common with
     | Yes _ ->
       User_error.raise
         [ Pp.textf
             "Another instance of dune%s has locked the _build directory. Refusing to \
              start a new watch server until no other instances of dune are running."
             (match lock_held_by with
              | Unknown -> ""
              | Pid_from_lockfile pid -> sprintf " (pid: %d)" pid)
         ]
     | No ->
       if not (Common.Builder.equal builder Common.Builder.default)
       then
         User_warning.emit
           [ Pp.textf
               "Your build request is being forwarded to a running Dune instance%s. Note \
                that certain command line arguments may be ignored."
               (match lock_held_by with
                | Unknown -> ""
                | Pid_from_lockfile pid -> sprintf " (pid: %d)" pid)
           ];
       Scheduler.go_without_rpc_server ~common ~config
       @@ fun () -> exec_building_via_rpc_server ~common ~prog ~args ~no_rebuild)
  | Ok () -> exec_building_directly ~common ~config ~context ~prog ~args ~no_rebuild
;;

let command = Cmd.v info term
