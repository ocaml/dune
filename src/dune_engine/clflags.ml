open Stdune

module Promote = struct
  type t =
    | Automatically
    | Never

  (* CR-soon rgrinberg: remove this *)
  let equal a b =
    match a, b with
    | Automatically, Automatically | Never, Never -> true
    | _, _ -> false
  ;;

  let repr =
    Repr.variant
      "promote"
      [ Repr.case0 "Automatically" ~test:(function
          | Automatically -> true
          | Never -> false)
      ; Repr.case0 "Never" ~test:(function
          | Never -> true
          | Automatically -> false)
      ]
  ;;

  let to_dyn = Repr.to_dyn repr
end

let target_exec = ref None
let report_errors_config = ref Report_errors_config.default
let stop_on_first_error = ref false
let capture_outputs = ref true

let debug_backtraces b =
  Dune_util.Report_error.report_backtraces b;
  Memo.Debug.track_locations_of_lazy_values := b
;;

let promote = ref None
let force = ref false
let always_show_command_line = ref false
let display = ref Display.Quiet
let can_go_in_shared_cache_default = ref false
let diff_command = ref None
let wait_for_filesystem_clock = ref false
