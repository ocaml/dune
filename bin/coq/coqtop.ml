open Import

let doc = "Execute a Coq toplevel with the local configuration."

let man =
  [ `S "DESCRIPTION"
  ; `P
      {|$(b,dune coq top FILE -- ARGS) runs the Coq toplevel to process the
          given $(b,FILE). The given arguments are completed according to the
          local configuration. This is equivalent to running $(b,coqtop ARGS)
          with a $(b,_CoqProject) file containing the local configurations
          from the $(b,dune) files, but does not require maintaining a
          $(b,_CoqProject) file.|}
  ; `Blocks Common.help_secs
  ]
;;

let info = Cmd.info "top" ~doc ~man

let term =
  let+ default_builder = Common.Builder.term
  and+ context =
    let doc = "Run the Coq toplevel in this build context." in
    Common.context_arg ~doc
  and+ coqtop =
    let doc = "Run the given toplevel command instead of the default." in
    Arg.(value & opt string "coqtop" & info [ "toplevel" ] ~docv:"CMD" ~doc)
  and+ coq_file_arg =
    Arg.(required & pos 0 (some string) None (Arg.info [] ~docv:"COQFILE"))
  and+ extra_args = Arg.(value & pos_right 0 string [] (Arg.info [] ~docv:"ARGS"))
  and+ no_rebuild =
    Arg.(
      value
      & flag
      & info [ "no-build" ] ~doc:"Don't rebuild dependencies before executing.")
  in
  let common, config =
    let builder =
      if no_rebuild then Common.Builder.forbid_builds default_builder else default_builder
    in
    Common.init builder
  in
  let coq_file_arg = Common.prefix_target common coq_file_arg |> Path.Local.of_string in
  let coqtop, argv, env =
    Scheduler.go ~common ~config
    @@ fun () ->
    let open Fiber.O in
    let* setup = Import.Main.setup () in
    let* setup = Memo.run setup in
    let sctx = Import.Main.find_scontext_exn setup ~name:context in
    let context = Dune_rules.Super_context.context sctx in
    let coq_file_build =
      Path.Build.append_local (Context.build_dir context) coq_file_arg
    in
    let dir =
      (match Path.Local.parent coq_file_arg with
       | None -> Path.Local.root
       | Some dir -> dir)
      |> Path.Build.append_local (Context.build_dir context)
    in
    let* coqtop, args, env =
      Build_system.run_exn
      @@ fun () ->
      let open Memo.O in
      let* (tr : Dune_rules.Dir_contents.triage) =
        Dune_rules.Dir_contents.triage sctx ~dir
      in
      let dir =
        match tr with
        | Group_part dir -> dir
        | Standalone_or_root _ -> dir
      in
      let* dc = Dune_rules.Dir_contents.get sctx ~dir in
      let* coq_src = Dune_rules.Dir_contents.coq dc in
      let coq_module =
        let source = coq_file_build in
        match Dune_rules.Coq.Coq_sources.find_module ~source coq_src with
        | Some m -> snd m
        | None ->
          let hints =
            [ Pp.textf "Is the file part of a stanza?"
            ; Pp.textf "Has the file been written to disk?"
            ]
          in
          User_error.raise
            ~hints
            [ Pp.textf "Cannot find file: %s" (coq_file_arg |> Path.Local.to_string) ]
      in
      let stanza = Dune_rules.Coq.Coq_sources.lookup_module coq_src coq_module in
      let args, use_stdlib, coq_lang_version, wrapper_name, mode =
        match stanza with
        | None ->
          User_error.raise
            [ Pp.textf
                "File not part of any stanza: %s"
                (coq_file_arg |> Path.Local.to_string)
            ]
        | Some (`Theory theory) ->
          ( Dune_rules.Coq.Coq_rules.coqtop_args_theory
              ~sctx
              ~dir
              ~dir_contents:dc
              theory
              coq_module
          , theory.buildable.use_stdlib
          , theory.buildable.coq_lang_version
          , Dune_rules.Coq.Coq_lib_name.wrapper (snd theory.name)
          , theory.buildable.mode )
        | Some (`Extraction extr) ->
          ( Dune_rules.Coq.Coq_rules.coqtop_args_extraction ~sctx ~dir extr coq_module
          , extr.buildable.use_stdlib
          , extr.buildable.coq_lang_version
          , "DuneExtraction"
          , extr.buildable.mode )
      in
      (* Run coqdep *)
      let* (_ : unit * Dep.Fact.t Dep.Map.t) =
        let deps_of =
          if no_rebuild
          then Action_builder.return ()
          else (
            let mode =
              match mode with
              | None -> Dune_rules.Coq.Coq_mode.VoOnly
              | Some mode -> mode
            in
            Dune_rules.Coq.Coq_rules.deps_of
              ~dir
              ~use_stdlib
              ~wrapper_name
              ~mode
              ~coq_lang_version
              coq_module)
        in
        Action_builder.evaluate_and_collect_facts deps_of
      in
      (* Get args *)
      let* (args, _) : string list * Dep.Fact.t Dep.Map.t =
        let* args = args in
        let dir = Path.external_ Path.External.initial_cwd in
        let args = Dune_rules.Command.expand ~dir (S args) in
        Action_builder.evaluate_and_collect_facts args.build
      in
      let* prog = Super_context.resolve_program_memo sctx ~dir ~loc:None coqtop in
      let prog = Action.Prog.ok_exn prog in
      let* () = Build_system.build_file prog in
      let+ env = Super_context.context_env sctx in
      Path.to_string prog, args, env
    in
    let argv =
      let topfile = Path.to_absolute_filename (Path.build coq_file_build) in
      (coqtop :: "-topfile" :: topfile :: args) @ extra_args
    in
    Fiber.return (coqtop, argv, env)
  in
  restore_cwd_and_execve common coqtop argv env
;;

let command = Cmd.v info term
