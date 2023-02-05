module Display = Dune_engine.Display

type t =
  { status_line : bool
  ; verbosity : Display.t
  }

let progress = { status_line = true; verbosity = Quiet }

let verbose = { status_line = true; verbosity = Verbose }

let short = { status_line = true; verbosity = Short }

let quiet = { status_line = false; verbosity = Quiet }

let short_no_status = { status_line = false; verbosity = Short }

(* Even though [status_line] is true by default in most of these, the status
    line is actually not shown if the output is redirected to a file or a
    pipe. *)
let all =
  [ ("progress", progress)
  ; ("verbose", verbose)
  ; ("short", short)
  ; ("quiet", quiet)
  ]

let to_dyn { status_line; verbosity } : Dyn.t =
  Record
    [ ("status_line", Dyn.Bool status_line)
    ; ("verbosity", Display.to_dyn verbosity)
    ]

let console_backend t =
  match t.status_line with
  | false -> Dune_console.Backend.dumb
  | true -> Dune_threaded_console.progress ()
