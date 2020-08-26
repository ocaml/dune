open! Stdune
open Import

let doc = "Start a merlin configuration server"

let man =
  [ `S "DESCRIPTION"
  ; `P
      {|$(b,dune ocaml-merlin) starts a server that can be queried to get
      .merlin information. It is meant to be used by Merlin itself and does not
      provide a user-friendly output.|}
  ; `Blocks Common.help_secs
  ; Common.footer
  ]

let info = Term.info "ocaml-merlin" ~doc ~man

let term =
  let+ common = Common.term in
  Common.set_common common ~targets:[];
  Scheduler.go ~common (fun () ->
      Dune_engine.File_tree.init ~recognize_jbuilder_projects:true
        ~ancestor_vcs:None;
      Dune_rules.Workspace.init ();
      Dune_rules.Merlin_server.start () |> Fiber.return)

let command = (term, info)
