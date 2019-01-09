open! Stdune
open Import
open Fiber.O

let doc = "Upgrade jbuilder projects to dune"

let man =
  [ `S "DESCRIPTION"
  ; `P {|$(b,dune upgrade) upgrade all the jbuilder projects
         in the workspace to dune|}
  ; `Blocks Common.help_secs
  ]

let info = Term.info "upgrade" ~doc ~man

let term =
  let%map common = Common.term
  in
  Common.set_common common ~targets:[];
  let log = Log.create common in
  Scheduler.go ~log ~common (fun () ->
    Import.Main.setup ~log common >>= fun setup ->
    Dune.Upgrader.upgrade setup.file_tree)

let command = term, info
