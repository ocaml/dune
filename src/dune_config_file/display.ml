open Dune_config
module Display = Dune_engine.Display

type t =
  | Simple of
      { status_line : bool
      ; verbosity : Display.t
      }
  | Tui

let progress = Simple { status_line = true; verbosity = Quiet }
let verbose = Simple { status_line = true; verbosity = Verbose }
let short = Simple { status_line = true; verbosity = Short }
let quiet = Simple { status_line = false; verbosity = Quiet }
let short_no_status = Simple { status_line = false; verbosity = Short }

(* Even though [status_line] is true by default in most of these, the status
   line is actually not shown if the output is redirected to a file or a
   pipe. *)
let all =
  [ "progress", progress; "verbose", verbose; "short", short; "quiet", quiet; "tui", Tui ]
;;

let to_dyn : t -> Dyn.t = function
  | Simple { verbosity; status_line } ->
    Variant
      ( "Simple"
      , [ Record
            [ "verbosity", Display.to_dyn verbosity; "status_line", Dyn.Bool status_line ]
        ] )
  | Tui -> Variant ("Tui", [])
;;

let console_backend = function
  | Tui -> Dune_tui.backend ()
  | Simple { status_line; _ } ->
    (match status_line with
     | false ->
       Dune_util.Terminal_signals.unblock ();
       Dune_console.Backend.dumb
     | true ->
       (match Config.(get threaded_console) with
        | `Enabled ->
          Dune_threaded_console.progress
            ~frames_per_second:(Dune_util.frames_per_second ())
        | `Disabled ->
          Dune_util.Terminal_signals.unblock ();
          Dune_console.Backend.progress))
;;
