open Import
module Utop = Dune_rules.Utop

let doc = "Load library in utop."

let man =
  [ `S "DESCRIPTION"
  ; `P {|$(b,dune utop DIR) build and run utop toplevel with libraries defined in DIR|}
  ; `Blocks Common.help_secs
  ]
;;

let info = Cmd.info "utop" ~doc ~man

let term =
  let+ builder = Common.Builder.term
  and+ dir = Arg.(value & pos 0 string "" & Arg.info [] ~docv:"DIR")
  and+ ctx_name = Common.context_arg ~doc:{|Select context where to build/run utop.|}
  and+ args = Arg.(value & pos_right 0 string [] (Arg.info [] ~docv:"ARGS")) in
  let common, config = Common.init builder in
  let dir = Common.prefix_target common dir in
  if not (Path.is_directory (Path.of_string dir))
  then User_error.raise [ Pp.textf "cannot find directory: %s" (String.maybe_quoted dir) ];
  let env, utop_path, requires =
    Scheduler.go ~common ~config (fun () ->
      let open Fiber.O in
      let* setup = Import.Main.setup () in
      Build_system.run_exn (fun () ->
        let open Memo.O in
        let* setup = setup in
        let context = Import.Main.find_context_exn setup ~name:ctx_name in
        let utop_target =
          let utop_target = Filename.concat dir Utop.utop_exe in
          Path.build (Path.Build.relative (Context.build_dir context) utop_target)
        in
        Build_system.file_exists utop_target
        >>= function
        | false ->
          User_error.raise
            [ Pp.textf "no library is defined in %s" (String.maybe_quoted dir) ]
        | true ->
          let* () = Build_system.build_file utop_target in
          let sctx = Import.Main.find_scontext_exn setup ~name:ctx_name in
          let* requires =
            let dir = Path.Build.relative (Context.build_dir context) dir in
            Utop.requires_under_dir sctx ~dir
          in
          let+ requires = Resolve.read_memo requires
          and+ env = Super_context.context_env sctx in
          env, Path.to_string utop_target, requires))
  in
  Hooks.End_of_build.run ();
  let env =
    Dune_rules.Lib_flags.L.toplevel_ld_paths requires
    |> Path.Set.fold
         ~f:(fun dir env -> Env_path.cons ~var:Ocaml.Env.caml_ld_library_path env ~dir)
         ~init:env
  in
  restore_cwd_and_execve common utop_path (utop_path :: args) env
;;

let command = Cmd.v info term
