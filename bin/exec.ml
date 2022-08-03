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

let info = Term.info "exec" ~doc ~man

type program_name =
  | String of string
  | Sw of Dune_lang.String_with_vars.t

let parse_program_name s =
  match Arg.conv_parser Arg.dep s with
  | Ok (File sw) when String.starts_with ~prefix:"%" s -> Sw sw
  | _ -> String s

type cli_item =
  | Program of program_name
  | Argument of string

(** Each item in the CLI is either interpreted as a program, or passed as a
    plain argument.

    The item in first position is always interpreted as a program, either in
    pform syntax or in string syntax. The other items are only interpreted as
    programs if they are in pform syntax. *)
let build_cli_items prog args =
  let prog = Program (parse_program_name prog) in
  let args =
    List.map args ~f:(fun s ->
        match parse_program_name s with
        | Sw _ as n -> Program n
        | String s -> Argument s)
  in
  prog :: args

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
        let cli_items = build_cli_items prog args in
        let+ argv =
          Build_system.run_exn (fun () ->
              Memo.List.map cli_items ~f:(function
                | Argument s -> Memo.return s
                | Program n ->
                  let open Memo.O in
                  let* prog =
                    match n with
                    | Sw sw ->
                      let+ path, _ =
                        Action_builder.run
                          (Target.expand_path_from_root (Common.root common)
                             ~setup context sw)
                          Eager
                      in
                      Path.to_string
                        (Path.build
                           (Path.Build.relative
                              (Dune_engine.Context_name.build_dir
                                 (Context.name context))
                              path))
                    | String s -> Memo.return s
                  in
                  let+ path =
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
                           let path =
                             Path.extend_basename path ~suffix:".exe"
                           in
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
                          let prog =
                            Path.extend_basename prog ~suffix:Bin.exe
                          in
                          Option.some_if (Path.exists prog) prog
                      with
                      | Some prog -> Memo.return prog
                      | None -> not_found ())
                  in
                  Path.to_string path))
        in
        let prog = List.hd argv in
        let env = Super_context.context_env sctx in
        (prog, argv, env))
  in
  restore_cwd_and_execve common prog argv env

let command = (term, info)
