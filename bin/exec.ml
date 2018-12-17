open Stdune
open Import

let doc =
  "Execute a command in a similar environment as if installation was performed."

let man =
  [ `S "DESCRIPTION"
  ; `P {|$(b,dune exec -- COMMAND) should behave in the same way as if you
          do:|}
  ; `Pre "  \\$ dune install\n\
          \  \\$ COMMAND"
  ; `P {|In particular if you run $(b,dune exec ocaml), you will have
          access to the libraries defined in the workspace using your usual
          directives ($(b,#require) for instance)|}
  ; `P {|When a leading / is present in the command (absolute path), then the
          path is interpreted as an absolute path|}
  ; `P {|When a / is present at any other position (relative path), then the
          path is interpreted as relative to the build context + current
          working directory (or the value of $(b,--root) when ran outside of
          the project root)|}
  ; `Blocks Common.help_secs
  ]

let info = Term.info "exec" ~doc ~man

let term =
  let%map common = Common.term
  and context =
    Common.context_arg ~doc:{|Run the command in this build context.|}
  and prog =
    Arg.(required
         & pos 0 (some string) None (Arg.info [] ~docv:"PROG"))
  and no_rebuild =
    Arg.(value & flag
         & info ["no-build"]
             ~doc:"don't rebuild target before executing")
  and args =
    Arg.(value
         & pos_right 0 string [] (Arg.info [] ~docv:"ARGS"))
  in
  Common.set_common common ~targets:[prog];
  let log = Log.create common in
  let setup =
    Scheduler.go ~log ~common (fun () -> Import.Main.setup ~log common) in
  let context = Import.Main.find_context_exn setup ~name:context in
  let prog_where =
    match Filename.analyze_program_name prog with
    | Absolute ->
      `This_abs (Path.of_string prog)
    | In_path ->
      `Search prog
    | Relative_to_current_dir ->
      let prog = Common.prefix_target common prog in
      `This_rel (Path.relative context.build_dir prog) in
  let targets = lazy (
    (match prog_where with
     | `Search p ->
       [Path.relative (Config.local_install_bin_dir ~context:context.name) p]
     | `This_rel p when Sys.win32 ->
       [p; Path.extend_basename p ~suffix:Bin.exe]
     | `This_rel p ->
       [p]
     | `This_abs p when Path.is_in_build_dir p ->
       [p]
     | `This_abs _ ->
       [])
    |> List.map ~f:(fun p -> Target.Path p)
    |> Target.resolve_targets_mixed ~log common setup
    |> List.concat_map ~f:(function
      | Ok targets -> targets
      | Error _ -> [])
  ) in
  let real_prog =
    if not no_rebuild then begin
      match Lazy.force targets with
      | [] -> ()
      | targets ->
        Scheduler.go ~log ~common (fun () -> do_build setup targets);
        Hooks.End_of_build.run ();
    end;
    match prog_where with
    | `Search prog ->
      let path = Config.local_install_bin_dir ~context:context.name :: context.path in
      Bin.which prog ~path
    | `This_rel prog
    | `This_abs prog ->
      if Path.exists prog then
        Some prog
      else if not Sys.win32 then
        None
      else
        let prog = Path.extend_basename prog ~suffix:Bin.exe in
        Option.some_if (Path.exists prog) prog
  in
  match real_prog, no_rebuild with
  | None, true ->
    begin match Lazy.force targets with
    | [] ->
      die "@{<Error>Error@}: Program %S not found!" prog
    | _::_ ->
      die "@{<Error>Error@}: Program %S isn't built yet \
           you need to build it first or remove the \
           --no-build option." prog
    end
  | None, false ->
    die "@{<Error>Error@}: Program %S not found!" prog
  | Some real_prog, _ ->
    let real_prog = Path.to_string real_prog     in
    let argv      = prog :: args in
    restore_cwd_and_execve common real_prog argv context.env

let command = term, info
