open Import
module Persistent = Dune_util.Persistent

let doc = "Dump the contents of a file stored in Dune's persistent database."

let man =
  [ `S "DESCRIPTION"
  ; `P
      {|Dump the contents of a file stored in Dune's persistent database in a human readable format.|}
  ; `Blocks Common.help_secs
  ]

let info = Term.info "dump" ~doc ~man

let term =
  let+ common = Common.term
  and+ file =
    Arg.(required & pos 0 (some Arg.path) None & Arg.info [] ~docv:"FILE")
  in
  let _config = Common.init common in
  let (Persistent.T ((module D), data)) =
    Persistent.load_exn (Arg.Path.path file)
  in
  Console.print [ Dyn.pp (D.to_dyn data) ]

let command = (term, info)
