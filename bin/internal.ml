open Import

let latest_lang_version =
  Cmd.v
    (Cmd.info "latest-lang-version")
    (let+ () = Term.const () in
     print_endline
       (Dune_lang.Syntax.greatest_supported_version_exn Stanza.syntax
        |> Dune_lang.Syntax.Version.to_string))
;;

let bootstrap_info =
  let doc = "Print the generated bootstrap info for Dune itself." in
  let info = Cmd.info "bootstrap-info" ~doc in
  let term =
    let+ builder = Common.Builder.term
    and+ context_name = Common.context_arg ~doc:(Some "Build context to use.") in
    let common, config = Common.init builder in
    Scheduler_setup.go_without_rpc_server ~common ~config (fun () ->
      let open Fiber.O in
      let* setup = Util.setup () in
      Build.build_memo_exn
      @@ fun () ->
      let open Memo.O in
      let* setup = setup in
      let context = Dune_rules.Main.find_context_exn setup ~name:context_name in
      let file = Path.Build.relative (Context.build_dir context) "bin/bootstrap-info" in
      let* () = Build_system.build_file (Path.build file) in
      let+ () = Memo.return (print_string (Io.read_file (Path.build file))) in
      ())
  in
  Cmd.v info term
;;

let group =
  Cmd.group
    (Cmd.info "internal")
    [ Internal_dump.command; latest_lang_version; bootstrap_info ]
;;
