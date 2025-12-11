(***********************************************)
(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* (c) INRIA 2019-2024                         *)
(* (c) Emilio J. Gallego Arias 2024-2025       *)
(* (c) CNRS 2025                               *)
(***********************************************)
(* Written by: Ali Caglayan                    *)
(* Written by: Emilio Jesús Gallego Arias      *)
(* Written by: Rudi Grinberg                   *)
(* Written by: Rodolphe Lepigre                *)
(***********************************************)

open Import

let doc = "Execute a Rocq toplevel with the local configuration."

let man =
  [ `S "DESCRIPTION"
  ; `P
      {|$(b,dune rocq top FILE -- ARGS) runs the Rocq toplevel to process the
          given $(b,FILE). The given arguments are completed according to the
          local configuration. This is equivalent to running $(b,rocq top ARGS)
          with a $(b,_RocqProject) file containing the local configurations
          from the $(b,dune) files, but does not require maintaining a
          $(b,_RocqProject) file.|}
  ; `Blocks Common.help_secs
  ]
;;

let info = Cmd.info "top" ~doc ~man

let term =
  let+ default_builder = Common.Builder.term
  and+ context =
    let doc = Some "Run the Rocq toplevel in this build context." in
    Common.context_arg ~doc
  and+ rocqtop =
    let doc = Some "Run the given toplevel command instead of the default." in
    Arg.(value & opt string "rocq" & info [ "toplevel" ] ~docv:"CMD" ~doc)
  and+ rocq_file_arg =
    Arg.(required & pos 0 (some string) None (Arg.info [] ~doc:None ~docv:"ROCQFILE"))
  and+ extra_args =
    Arg.(value & pos_right 0 string [] (Arg.info [] ~doc:None ~docv:"ARGS"))
  and+ no_rebuild =
    Arg.(
      value
      & flag
      & info [ "no-build" ] ~doc:(Some "Don't rebuild dependencies before executing."))
  in
  let common, config =
    let builder =
      if no_rebuild then Common.Builder.forbid_builds default_builder else default_builder
    in
    Common.init builder
  in
  let rocq_file_arg = Common.prefix_target common rocq_file_arg |> Path.Local.of_string in
  let rocqtop, args, env =
    Scheduler.go_with_rpc_server ~common ~config
    @@ fun () ->
    let open Fiber.O in
    let* setup = Import.Main.setup () in
    let* setup = Memo.run setup in
    let sctx = Import.Main.find_scontext_exn setup ~name:context in
    let context = Dune_rules.Super_context.context sctx in
    let rocq_file_build =
      Path.Build.append_local (Context.build_dir context) rocq_file_arg
    in
    let dir =
      (match Path.Local.parent rocq_file_arg with
       | None -> Path.Local.root
       | Some dir -> dir)
      |> Path.Build.append_local (Context.build_dir context)
    in
    let* rocqtop, rocq_arg, args, env =
      build_exn
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
      let* rocq_src = Dune_rules.Dir_contents.rocq dc in
      let rocq_module =
        let source = rocq_file_build in
        match Dune_rules.Rocq.Rocq_sources.find_module ~source rocq_src with
        | Some m -> snd m
        | None ->
          let hints =
            [ Pp.textf "Is the file part of a stanza?"
            ; Pp.textf "Has the file been written to disk?"
            ]
          in
          User_error.raise
            ~hints
            [ Pp.textf "Cannot find file: %s" (rocq_file_arg |> Path.Local.to_string) ]
      in
      let stanza = Dune_rules.Rocq.Rocq_sources.lookup_module rocq_src rocq_module in
      let args, use_stdlib, wrapper_name, mode =
        match stanza with
        | None ->
          User_error.raise
            [ Pp.textf
                "File not part of any stanza: %s"
                (rocq_file_arg |> Path.Local.to_string)
            ]
        | Some (`Theory theory) ->
          ( Dune_rules.Rocq.Rocq_rules.rocqtop_args_theory
              ~sctx
              ~dir
              ~dir_contents:dc
              theory
              rocq_module
          , theory.buildable.use_stdlib
          , Dune_rules.Rocq.Rocq_lib_name.wrapper (snd theory.name)
          , theory.buildable.mode )
        | Some (`Extraction extr) ->
          ( Dune_rules.Rocq.Rocq_rules.rocqtop_args_extraction ~sctx ~dir extr rocq_module
          , extr.buildable.use_stdlib
          , "DuneExtraction"
          , extr.buildable.mode )
      in
      (* Run rocqdep *)
      let* (_ : unit * Dep.Fact.t Dep.Map.t) =
        let deps_of =
          if no_rebuild
          then Action_builder.return ()
          else (
            let mode =
              match mode with
              | None -> Dune_rules.Rocq.Rocq_mode.VoOnly
              | Some mode -> mode
            in
            Dune_rules.Rocq.Rocq_rules.deps_of
              ~dir
              ~use_stdlib
              ~wrapper_name
              ~mode
              rocq_module)
        in
        Action_builder.evaluate_and_collect_facts deps_of
      in
      let real_binary, rocq_arg =
        if String.equal "rocq" rocqtop then "rocq", [ "top" ] else rocqtop, []
      in
      let* prog = Super_context.resolve_program_memo sctx ~dir ~loc:None real_binary in
      let prog = Action.Prog.ok_exn prog in
      let* () = Build_system.build_file prog in
      (* Get args *)
      let* (args, _) : string list * Dep.Fact.t Dep.Map.t =
        let* args = args in
        let dir = Path.external_ Path.External.initial_cwd in
        let args =
          Dune_rules.Command.expand ~dir (S args)
          |> Action_builder.With_targets.map ~f:Appendable_list.to_list
        in
        Action_builder.evaluate_and_collect_facts args.build
      in
      let+ env = Super_context.context_env sctx in
      Path.to_string prog, rocq_arg, args, env
    in
    (* Careful about the first argument to "rocq" *)
    let args =
      let topfile = Path.to_absolute_filename (Path.build rocq_file_build) in
      rocq_arg @ ("-topfile" :: topfile :: args) @ extra_args
    in
    Fiber.return (rocqtop, args, env)
  in
  restore_cwd_and_execve (Common.root common) rocqtop args env
;;

let command = Cmd.v info term
