open Stdune
open Import

let doc = "Execute the Coq toplevel with the local configuration."

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

let info = Term.info "top" ~doc ~man

let term =
  let+ common = Common.term
  and+ context =
    Common.context_arg ~doc:{|Run the Coq toplevel in this build context.|}
  and+ coqtop =
    let doc = "Run the given toplevel command instead of the default." in
    Arg.(value & opt string "coqtop" & info [ "toplevel" ] ~docv:"CMD" ~doc)
  and+ coq_file_arg =
    Arg.(required & pos 0 (some string) None (Arg.info [] ~docv:"COQFILE"))
  and+ extra_args =
    Arg.(value & pos_right 0 string [] (Arg.info [] ~docv:"ARGS"))
  in
  let config = Common.init common in
  let coqtop, argv, env =
    Scheduler.go ~common ~config (fun () ->
        let open Fiber.O in
        let* setup = Import.Main.setup () in
        let* setup = Memo.run setup in
        let sctx = Import.Main.find_scontext_exn setup ~name:context in
        let context = Dune_rules.Super_context.context sctx in
        (* Try to compute a relative path if we got an absolute path. *)
        let coq_file_arg =
          if Filename.is_relative coq_file_arg then coq_file_arg
          else
            let cwd = Path.external_ Path.External.initial_cwd in
            let file =
              (* Best-effort symbolic link unfolding. *)
              let file = Fpath.follow_symlinks coq_file_arg in
              Option.value file ~default:coq_file_arg
            in
            let file = Path.of_filename_relative_to_initial_cwd file in
            let cwd = Path.to_string cwd in
            let file = Path.to_string file in
            match String.drop_prefix ~prefix:(cwd ^ "/") file with
            | None -> coq_file_arg
            | Some s -> s
        in
        let coq_file_build =
          let p = Common.prefix_target common coq_file_arg in
          Path.Build.relative context.build_dir p
        in
        let dir =
          let dir = Filename.dirname coq_file_arg in
          let p = Common.prefix_target common dir in
          Path.Build.relative context.build_dir p
        in
        let* coqtop, args =
          let open Memo.O in
          Build_system.run_exn (fun () ->
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
                match Dune_rules.Coq_sources.find_module ~source coq_src with
                | Some m -> snd m
                | None ->
                  let hints =
                    [ Pp.textf "is the file part of a stanza?"
                    ; Pp.textf "has the file been written to disk?"
                    ]
                  in
                  User_error.raise ~hints
                    [ Pp.textf "cannot find file: %s" coq_file_arg ]
              in
              let stanza =
                Dune_rules.Coq_sources.lookup_module coq_src coq_module
              in
              let* args, boot_type =
                match stanza with
                | None ->
                  User_error.raise
                    [ Pp.textf "file not part of any stanza: %s" coq_file_arg ]
                | Some (`Theory theory) ->
                  Dune_rules.Coq_rules.coqtop_args_theory ~sctx ~dir
                    ~dir_contents:dc theory coq_module
                | Some (`Extraction extr) ->
                  Dune_rules.Coq_rules.coqtop_args_extraction ~sctx ~dir
                    ~dir_contents:dc extr
              in
              let* (_ : unit * Dep.Fact.t Dep.Map.t) =
                let deps =
                  let boot_type = Resolve.Memo.return boot_type in
                  Dune_rules.Coq_rules.deps_of ~dir ~boot_type coq_module
                in
                Action_builder.run deps Eager
              in
              let* (args, _) : string list * Dep.Fact.t Dep.Map.t =
                let args =
                  let dir = Path.external_ Path.External.initial_cwd in
                  Dune_rules.Command.expand ~dir (S args)
                in
                Action_builder.run args.build Eager
              in
              let* prog =
                Super_context.resolve_program sctx ~dir ~loc:None coqtop
              in
              let prog = Action.Prog.ok_exn prog in
              let+ (_ : Digest.t) = Build_system.build_file prog in
              (Path.to_string prog, args))
        in
        let argv =
          let topfile = Path.to_absolute_filename (Path.build coq_file_build) in
          (coqtop :: "-topfile" :: topfile :: args) @ extra_args
        in
        let env = Super_context.context_env sctx in
        Fiber.return (coqtop, argv, env))
  in
  restore_cwd_and_execve common coqtop argv env

let command = (term, info)
