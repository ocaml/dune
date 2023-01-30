open Import

type verbosity =
  | Quiet
  | Short
  | Verbose

type t =
  { status_line : bool
  ; verbosity : verbosity
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

let verbosity_to_dyn : verbosity -> Dyn.t = function
  | Quiet -> Variant ("Quiet", [])
  | Short -> Variant ("Short", [])
  | Verbose -> Variant ("Verbose", [])

let to_dyn { status_line; verbosity } : Dyn.t =
  Record
    [ ("status_line", Dyn.Bool status_line)
    ; ("verbosity", verbosity_to_dyn verbosity)
    ]

let console_backend t =
  match t.status_line with
  | false -> Console.Backend.dumb
  | true -> Console.Backend.progress_threaded ()
