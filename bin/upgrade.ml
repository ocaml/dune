open! Stdune
open Import

let doc = "Upgrade jbuilder projects to dune"

let man =
  [ `S "DESCRIPTION"
  ; `P
      {|$(b,dune upgrade) upgrade all the jbuilder projects
         in the workspace to Dune|}
  ; `Blocks Common.help_secs
  ]

let info = Cmd.info "upgrade" ~doc ~man

let term =
  let+ common = Common.term in
  let config = Common.init common in
  Scheduler.go ~common ~config (fun () -> Dune_upgrader.upgrade ())

let command = Cmd.v info term
