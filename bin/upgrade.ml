open! Stdune
open Import

let doc = "Upgrade jbuilder projects to dune"

let man =
  [ `S "DESCRIPTION"
  ; `P {|$(b,dune upgrade) upgrade all the jbuilder projects
         in the workspace to dune|}
  ; `Blocks Common.help_secs
  ]

let info = Term.info "upgrade" ~doc ~man

let term =
  let+ common = Common.term in
  Common.set_common common ~targets:[];
  Scheduler.go ~common (fun () ->
    Dune.Upgrader.upgrade (Dune.File_tree.load Path.Source.root
                             ~warn_when_seeing_jbuild_file:false
                             ~ancestor_vcs:None))

let command = term, info
