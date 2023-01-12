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
  let config = Common.init common in
  let prog, argv, env =
    Scheduler.go ~common ~config (fun () ->
        let open Fiber.O in
        let* setup = Import.Main.setup () in
        let* setup = Memo.run setup in
        let sctx = Import.Main.find_scontext_exn setup ~name:context in
        let context = Dune_rules.Super_context.context sctx in
        let dir =
          Path.Build.relative context.build_dir (Common.prefix_target common "")
        in
        let build_prog p =
          let open Memo.O in
          if no_rebuild then
            if Path.exists p then Memo.return p
            else
              User_error.raise
                [ Pp.textf
                    "Program %S isn't built yet. You need to build it first or \
                     remove the --no-build option."
                    prog
                ]
          else
            let+ () = Build_system.build_file p in
            p
        in
        let not_found () =
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
        in
        let* prog =
          let open Memo.O in
          Build_system.run_exn (fun () ->
              match Filename.analyze_program_name prog with
              | In_path -> (
                Super_context.resolve_program sctx ~dir ~loc:None prog
                >>= function
                | Error (_ : Action.Prog.Not_found.t) -> not_found ()
                | Ok prog -> build_prog prog)
              | Relative_to_current_dir -> (
                let path =
                  Path.relative_to_source_in_build_or_external ~dir prog
                in
                (Build_system.file_exists path >>= function
                 | true -> Memo.return (Some path)
                 | false -> (
                   if not (Filename.check_suffix prog ".exe") then
                     Memo.return None
                   else
                     let path = Path.extend_basename path ~suffix:".exe" in
                     Build_system.file_exists path >>= function
                     | true -> Memo.return (Some path)
                     | false -> Memo.return None))
                >>= function
                | Some path -> build_prog path
                | None -> not_found ())
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
                | None -> not_found ()))
        in
        let prog = Path.to_string prog in
        let argv = prog :: args in
        let env = Super_context.context_env sctx in
        Fiber.return (prog, argv, env))
  in
  restore_cwd_and_execve common prog argv env

let command = Cmd.v info term
