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

let not_found ~dir ~prog =
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
  User_error.raise
    ~hints
    [ Pp.concat
        ~sep:Pp.space
        [ Pp.text "Program"; User_message.command prog; Pp.text "not found!" ]
    ]
;;

let build_prog ~no_rebuild ~prog p =
  if no_rebuild
  then
    if Path.exists p
    then Memo.return p
    else
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
     | Error (_ : Action.Prog.Not_found.t) -> not_found ~dir ~prog
     | Ok p -> build_prog ~no_rebuild ~prog p)
  | Relative_to_current_dir ->
    let path = Path.relative_to_source_in_build_or_external ~dir prog in
    Build_system.file_exists path
    >>= (function
     | true -> Memo.return (Some path)
     | false ->
       if not (Filename.check_suffix prog ".exe")
       then Memo.return None
       else (
         let path = Path.extend_basename path ~suffix:".exe" in
         Build_system.file_exists path
         >>| function
         | true -> Some path
         | false -> None))
    >>= (function
     | Some path -> build_prog ~no_rebuild ~prog path
     | None -> not_found ~dir ~prog)
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
     | None -> not_found ~dir ~prog)
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
  @@ Dune_engine.Process.run_external_in_out
       ~dir:(Path.of_string Fpath.initial_cwd)
       ~env
       path
       args
  >>| function
  | 0 -> ()
  | exit_code -> on_exit exit_code
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

let command = Cmd.v info term
