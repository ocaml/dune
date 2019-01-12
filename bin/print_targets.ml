open! Stdune
open Import

let doc = "Print available targets"

let man =
  [ `S "DESCRIPTION"
  ; `P {|$(b,dune targets) prints the available targets|}
  ; `Blocks Common.help_secs
  ]

let info = Term.info "targets" ~doc ~man

let term =
  let+ common = Common.term in
  Common.set_common common ~targets:[] ;
  Scheduler.go ~common (fun () ->
      let open Fiber.O in
      let* setup = Import.Main.setup common in
      let targets = Target.targets_of_path setup Path.root in
      List.iter targets ~f:print_endline ;
      Fiber.return ()
    )

let command = (term, info)
