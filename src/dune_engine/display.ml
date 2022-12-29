open Import

type verbosity =
  | Quiet
  | Short
  | Verbose

type t =
  { status_line : bool
  ; verbosity : verbosity
  }

(* Even though [status_line] is true by default in most of these, the status
    line is actually not shown if the output is redirected to a file or a
    pipe. *)
let all =
  [ ("progress", { verbosity = Quiet; status_line = true })
  ; ("verbose", { verbosity = Verbose; status_line = true })
  ; ("short", { verbosity = Short; status_line = true })
  ; ("quiet", { verbosity = Quiet; status_line = false })
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
  | true -> Console.Backend.progress ()
