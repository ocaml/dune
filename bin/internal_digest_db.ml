open Import

let doc = "Inspect Dune's persistent digest database."

let help_secs =
  [ `S "DESCRIPTION"
  ; `P
      {|Inspect the persistent database backing Dune's cached file digests. The check subcommand only reports entries whose cached digest is actually wrong.|}
  ; `Blocks Common.help_secs
  ]
;;

let path_args = Arg.(value & pos_all Arg.path [] & info [] ~docv:"PATH" ~doc:None)

let run builder paths ~f =
  let _common, _config = Common.init builder in
  let paths =
    List.map paths ~f:(fun path ->
      Arg.Path.path path |> Path.Expert.try_localize_external)
  in
  Console.print [ Dyn.pp (f paths) ]
;;

let dump =
  let info =
    let doc = "Dump the digest database or selected entries." in
    Cmd.info "dump" ~doc ~man:help_secs
  in
  Cmd.v info
  @@
  let+ builder = Common.Builder.term_no_trace_no_pkg
  and+ paths = path_args in
  run builder paths ~f:Dune_engine.Fs_memo.Debug.dump_digest_db
;;

let check =
  let info =
    let doc = "Report invalid or stale cached digests." in
    Cmd.info "check" ~doc ~man:help_secs
  in
  Cmd.v info
  @@
  let+ builder = Common.Builder.term_no_trace_no_pkg
  and+ paths = path_args in
  run builder paths ~f:Dune_engine.Fs_memo.Debug.check_digest_db
;;

let command = Cmd.group (Cmd.info "digest-db" ~doc) [ dump; check ]
