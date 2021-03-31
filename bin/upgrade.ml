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

let info = Term.info "upgrade" ~doc ~man

let term =
  let+ common = Common.term in
  let config = Common.set_common common in
  Scheduler.go ~common ~config (fun () ->
      Dune_engine.Source_tree.init ~recognize_jbuilder_projects:true
        ~ancestor_vcs:None;
      Dune_upgrader.upgrade ())

let command = (term, info)
