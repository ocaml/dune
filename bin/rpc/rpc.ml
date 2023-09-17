open Import

let info =
  let doc = "Dune's RPC mechanism. Experimental." in
  let man =
    [ `S "DESCRIPTION"
    ; `P {|This is experimental. do not use|}
    ; `Blocks Common.help_secs
    ]
  in
  Cmd.info "rpc" ~doc ~man
;;

let group = Cmd.group info [ Status.cmd; Build.cmd; Ping.cmd ]
