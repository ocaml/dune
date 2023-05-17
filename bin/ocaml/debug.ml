open Stdune
open Import

let doc =
  "Debug a bytecode program in a similar environment as if installation was \
   performed."

let man =
  [ `S "DESCRIPTION"
  ; `P
      {|$(b,dune debug -- COMMAND) should behave in the same way as if you
          do:|}
  ; `Pre "  \\$ dune install\n  \\$ ocamldebug COMMAND"
  ; `P
      {|In particular if you run $(b,dune exec ocaml), you will have
          access to the libraries defined in the workspace using your usual
          directives ($(b,#require) for instance). In addition the
          BUILD_PATH_PREFIX_MAP is set to map abstract path names
          to paths in your runtime environment, so tha the debugger
          can find your source code.|}
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
      [ ("Debug the executable named `my_exec'", "dune debug my_exec")
      ; ( "Run the executable defined in `foo.ml' with the argument `arg'"
        , "dune exec -- ./foo.exe arg" )
      ]
  ]

let info = Cmd.info "debug" ~doc ~man

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
    ; ocamldebug : string Fiber.t
    ; debug_args : string list
    ; maps : (Build_path_prefix_map.map * Build_path_prefix_map.map) Fiber.t
    ; args : string list
    ; env : Env.t Fiber.t
    ; get_path_and_build_if_necessary : (unit -> Path.t Memo.t) Fiber.t
    }

  let init ~common ~context ~debugger ~debug_args ~no_rebuild ~prog ~args =
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
    let ocamldebug =
      Fiber.map sctx ~f:(fun sctx ->
          let context = Dune_rules.Super_context.context sctx in
          match debugger with
          | Some debugger -> (
            if Sys.file_exists debugger then debugger
            else
              match Bin.which ~path:context.path debugger with
              | Some debugger -> Path.to_string debugger
              | None -> Dune_engine.Utils.program_not_found debugger ~loc:None)
          | None -> Path.to_string context.ocaml.ocamldebug)
    in
    let maps =
      Fiber.map sctx ~f:(fun sctx ->
          let context = Dune_rules.Super_context.context sctx in
          let src_dir = Sys.getcwd () in
          let build_dir =
            Filename.concat src_dir (Path.Build.to_string context.build_dir)
          in
          let install_root =
            context.ocaml.bin_dir |> Path.parent_exn |> Path.to_string
          in
          let forward_map =
            [ Some
                { Build_path_prefix_map.target = "/workspace_root"
                ; source = install_root
                }
            ; Some
                { Build_path_prefix_map.target = "/workspace_root"
                ; source = src_dir
                }
            ; Some
                { Build_path_prefix_map.target = "/workspace_root"
                ; source = build_dir
                }
            ]
          in
          let inverse_map =
            [ Some
                { Build_path_prefix_map.source = "/workspace_root"
                ; target = install_root
                }
            ; Some
                { Build_path_prefix_map.source = "/workspace_root"
                ; target = build_dir
                }
            ; Some
                { Build_path_prefix_map.source = "/workspace_root"
                ; target = src_dir
                }
            ]
          in
          (forward_map, inverse_map))
    in

    { common
    ; config
    ; ocamldebug
    ; debug_args
    ; maps
    ; env
    ; args
    ; get_path_and_build_if_necessary
    }

  let run_once
      { common
      ; config
      ; ocamldebug
      ; debug_args
      ; maps
      ; env
      ; args
      ; get_path_and_build_if_necessary
      ; _
      } =
    Scheduler.go ~common ~config @@ fun () ->
    let open Fiber.O in
    let* get_path_and_build_if_necessary = get_path_and_build_if_necessary in
    let* ocamldebug = ocamldebug in
    let* env = env in
    let* forward_map, inverse_map = maps in
    let+ path = Build_system.run_exn get_path_and_build_if_necessary in
    Log.info [ Pp.textf "In run_once" ];
    let prog = Path.to_string path in
    let args = prog :: args in
    let prog = ocamldebug in
    let argv = (prog :: debug_args) @ args in
    let env =
      Dune_util.Build_path_prefix_map0.extend_build_path_prefix_map env
        `New_rules_have_precedence forward_map
    in
    let env =
      Dune_util.Build_path_prefix_map0.extend_deploy_path_prefix_map env
        `New_rules_have_precedence inverse_map
    in
    Log.info [ Pp.textf "prog=%s,argv=%s" prog (String.concat ~sep:"||" argv) ];
    restore_cwd_and_execve common prog argv env
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
  and+ emacs =
    Arg.(
      value & flag
      & info [ "emacs" ]
          ~doc:"For running the debugger under emacs; implies -machine-readable")
  and+ machine_readable =
    Arg.(
      value & flag
      & info [ "machine-readable" ]
          ~doc:"Print information in a format more suitable for machines")
  and+ no_version =
    Arg.(
      value & flag
      & info [ "no-version" ] ~doc:"Do not print version at startup")
  and+ no_prompt =
    Arg.(value & flag & info [ "no-prompt" ] ~doc:"Suppress all prompts")
  and+ no_time =
    Arg.(value & flag & info [ "no-time" ] ~doc:"Do not print times")
  and+ topdirs_path =
    Arg.(
      value & flag
      & info [ "topdirs-path" ]
          ~doc:"Set path to the directory containing topdirs.cmi")
  and+ no_breakpoint_message =
    Arg.(
      value & flag
      & info
          [ "no-breakpoint-message" ]
          ~doc:"Do not print message at breakpoint setup and removal")
  and+ debugger_help =
    Arg.(
      value & flag
      & info [ "debugger-help" ] ~doc:"Print the debugger's help message")
  and+ debugger_version =
    Arg.(
      value & flag
      & info [ "debugger-version" ] ~doc:"Print the debugger's version")
  and+ cd_dir =
    Arg.(
      let doc = "Change working directory" in
      value & opt (some string) None & info [ "cd" ] ~docv:"<dir>" ~doc)
  and+ socket =
    Arg.(
      let doc = "Set the name of the communication socket" in
      value & opt (some string) None & info [ "s" ] ~docv:"<filename>" ~doc)
  and+ count =
    Arg.(
      let doc = "Set max number of checkpoints kept" in
      value & opt (some int) None & info [ "c" ] ~docv:"COUNT" ~doc)
  and+ includes =
    Arg.(
      let doc =
        "Add <dir> to the list of include directories."
        ^ " May be used multiple times"
      in
      value & opt_all (some dir) [] & info [ "I" ] ~docv:"<dir>" ~doc)
  and+ debugger =
    Arg.(
      let doc = "A debugger to use instead of ocamldebug" in
      value & opt (some string) None & info [ "debugger" ] ~docv:"DEBUGGER" ~doc)
  and+ debugger_option =
    Arg.(
      let doc = "Add debugger options <opt>. May be used multiple times." in
      value & opt_all (some string) [] & info [ "do" ] ~docv:"<opt>" ~doc)
  and+ args = Arg.(value & pos_right 0 string [] (Arg.info [] ~docv:"ARGS")) in

  let debug_args = ref [] in
  if emacs then debug_args := "-emacs" :: !debug_args;
  if machine_readable then debug_args := "-machine_readable" :: !debug_args;
  if no_version then debug_args := "-no-version" :: !debug_args;
  if no_prompt then debug_args := "-no-prompt" :: !debug_args;
  if no_time then debug_args := "-no-time" :: !debug_args;
  if topdirs_path then debug_args := "-topdirs-path" :: !debug_args;
  if no_breakpoint_message then
    debug_args := "-no_breakpoint_message" :: !debug_args;
  if debugger_help then debug_args := "-help" :: !debug_args;
  if debugger_version then debug_args := "-version" :: !debug_args;
  (match cd_dir with
  | None -> ()
  | Some dir -> debug_args := "-cd" :: dir :: !debug_args);
  (match count with
  | None -> ()
  | Some c -> debug_args := "-c" :: Int.to_string c :: !debug_args);
  (match socket with
  | None -> ()
  | Some dir -> debug_args := "-s" :: dir :: !debug_args);
  List.iter includes ~f:(fun dir_opt ->
      match dir_opt with
      | None -> ()
      | Some dir -> debug_args := "-I" :: dir :: !debug_args);
  List.iter debugger_option ~f:(fun dir_opt ->
      match dir_opt with
      | None -> ()
      | Some str -> debug_args := str :: !debug_args);
  debug_args := List.rev !debug_args;

  (* TODO we should make sure to finalize the current backend before exiting dune.
     For watch mode, we should finalize the backend and then restart it in between
     runs. *)
  let exec_context =
    Exec_context.init ~common ~context ~debugger ~debug_args:!debug_args
      ~no_rebuild ~prog ~args
  in
  (* We do not support watch mode for the debugger. *)
  match Common.watch common with
  | Yes Passive ->
    User_error.raise [ Pp.textf "passive watch mode is unsupported by debug" ]
  | Yes Eager ->
    User_error.raise [ Pp.textf "eager watch mode is unsupported by debug" ]
  | No -> Exec_context.run_once exec_context

let command = Cmd.v info term
