open Import
module Utop = Dune_rules.Utop

let doc = "Load library in utop."

let man =
  [ `S "DESCRIPTION"
  ; `P {|$(b,dune utop DIR) build and run utop toplevel with libraries defined in DIR|}
  ; `Blocks Common.help_secs
  ]
;;

let man_xrefs = [ `Cmd "top" ]
let info = Cmd.info "utop" ~man_xrefs ~doc ~man

let lock_utop_if_dev_tool_enabled () =
  match Lazy.force Lock_dev_tool.is_enabled with
  | false -> Memo.return ()
  | true -> Lock_dev_tool.lock_dev_tool Utop
;;

let term =
  let+ builder = Common.Builder.term
  (* CR-someday Alizter: document this option *)
  and+ dir = Arg.(value & pos 0 string "" & Arg.info [] ~docv:"DIR" ~doc:None)
  and+ ctx_name =
    Common.context_arg ~doc:(Some {|Select context where to build/run utop.|})
  (* CR-someday Alizter: document this option *)
  and+ args = Arg.(value & pos_right 0 string [] (Arg.info [] ~docv:"ARGS" ~doc:None)) in
  let common, config = Common.init builder in
  let dir = Common.prefix_target common dir in
  if not (Path.is_directory (Path.of_string dir))
  then User_error.raise [ Pp.textf "cannot find directory: %s" (String.maybe_quoted dir) ];
  let env, utop_path =
    Scheduler.go_with_rpc_server ~common ~config (fun () ->
      let open Fiber.O in
      let* setup = Import.Main.setup () in
      build_exn (fun () ->
        let open Memo.O in
        let* setup = setup in
        let context = Import.Main.find_context_exn setup ~name:ctx_name in
        let utop_target_path filename =
          Path.build
            (Path.Build.relative
               (Context.build_dir context)
               (Filename.concat dir filename))
        in
        let utop_exe = utop_target_path Utop.utop_exe in
        let utop_findlib_conf = utop_target_path Utop.utop_findlib_conf in
        let* () =
          (* Calling [Build_system.file_exists] has the side effect of checking
             and memoizing whether or not the utop dev tool lockdir exists.
             thus if we generate the lockdir any later than this point, dune
             will not observe the fact that it now exists. *)
          lock_utop_if_dev_tool_enabled ()
        in
        Build_system.file_exists utop_exe
        >>= function
        | false ->
          User_error.raise
            [ Pp.textf "no library is defined in %s" (String.maybe_quoted dir) ]
        | true ->
          let* () = Build_system.build_file utop_exe in
          let utop_dev_tool_lock_dir_exists =
            Lazy.force Utop.utop_dev_tool_lock_dir_exists
          in
          let* () =
            if utop_dev_tool_lock_dir_exists
            then
              (* Generate the custom findlib.conf file needed when utop is run
                 as a dev tool. *)
              Build_system.build_file utop_findlib_conf
            else Memo.return ()
          in
          let sctx = Import.Main.find_scontext_exn setup ~name:ctx_name in
          let* requires =
            let dir = Path.Build.relative (Context.build_dir context) dir in
            Utop.requires_under_dir sctx ~dir
          in
          let+ requires = Resolve.read_memo requires
          and+ lib_config =
            let+ ocaml = Context.ocaml context in
            ocaml.lib_config
          and+ env = Super_context.context_env sctx in
          let env =
            Dune_rules.Lib_flags.L.toplevel_ld_paths requires lib_config
            |> Path.Set.fold
                 ~f:(fun dir env ->
                   Env_path.cons ~var:Root.Ocaml.Env.caml_ld_library_path env ~dir)
                 ~init:env
          in
          let env =
            if utop_dev_tool_lock_dir_exists
            then
              (* If there's a utop lockdir then dune will have built utop as a
                 dev tool. In order for it to run correctly dune needed to
                 generate a custom findlib.conf that contains the locations of
                 all of utop's dependencies within the project's _build
                 directory. Setting this environment variable causes the custom
                 findlib.conf file to be used instead of the default
                 findlib.conf. *)
              Env.add env ~var:"OCAMLFIND_CONF" ~value:(Path.to_string utop_findlib_conf)
            else env
          in
          env, Path.to_string utop_exe))
  in
  Hooks.End_of_build.run ();
  restore_cwd_and_execve (Common.root common) utop_path args env
;;

let command = Cmd.v info term
