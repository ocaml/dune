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

let group = Cmd.group info [ Rpc_status.cmd; Rpc_build.cmd; Rpc_ping.cmd ]

module Build = Rpc_build
