module Verbosity = struct
  type t =
    | Quiet
    | Short
    | Verbose

  let to_dyn : t -> Dyn.t = function
    | Quiet -> Variant ("Quiet", [])
    | Short -> Variant ("Short", [])
    | Verbose -> Variant ("Verbose", [])
end

type t =
  | Simple of
      { status_line : bool
      ; verbosity : Verbosity.t
      }
  | Tui

(* Even though [status_line] is true by default in most of these, the status
   line is actually not shown if the output is redirected to a file or a
   pipe. *)
let all =
  [ ("progress", Simple { verbosity = Quiet; status_line = true })
  ; ("quiet", Simple { verbosity = Quiet; status_line = false })
  ; ("short", Simple { verbosity = Short; status_line = true })
  ; ("verbose", Simple { verbosity = Verbose; status_line = true })
  ; ("tui", Tui)
  ]

let to_dyn : t -> Dyn.t = function
  | Simple { verbosity; status_line } ->
    Dyn.Record
      [ ("verbosity", Verbosity.to_dyn verbosity)
      ; ("status_line", Dyn.Bool status_line)
      ]
  | Tui -> Variant ("Tui", [])

let console_backend = function
  | Tui -> Dune_console.Backend.tui ()
  | Simple { status_line; _ } -> (
    match status_line with
    | false -> Dune_console.Backend.dumb
    | true -> Dune_console.Backend.progress ())
