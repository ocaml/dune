module Promote = struct
  type t =
    | Automatically
    | Never

  let to_dyn = function
    | Automatically -> Dyn.variant "Automatically" []
    | Never -> Dyn.variant "Never" []
  ;;
end

let report_errors_config = ref Report_errors_config.default
let stop_on_first_error = ref false
let debug_fs_cache = ref false
let capture_outputs = ref true

let debug_backtraces b =
  Dune_util.Report_error.report_backtraces b;
  Memo.Debug.track_locations_of_lazy_values := b
;;

let debug_load_dir = ref false
let promote = ref None
let force = ref false
let always_show_command_line = ref false
let display = ref Display.Quiet
let can_go_in_shared_cache_default = ref false
