open! Stdune
open Import

let doc = "Upgrade projects across major Dune versions."

let man =
  [ `S "DESCRIPTION"
  ; `P
      "$(b,dune upgrade) upgrades all the projects in the workspace to the \
       latest major version of Dune"
  ; `Blocks Common.help_secs
  ]

let info = Cmd.info "upgrade" ~doc ~man

let term =
  let+ common = Common.term in
  let config = Common.init common in
  Scheduler.go ~common ~config (fun () -> Dune_upgrader.upgrade ())

let command = Cmd.v info term
