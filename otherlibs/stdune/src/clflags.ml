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
let promote = ref None
let force = ref false
let always_show_command_line = ref false
let display = ref Display.Quiet
let can_go_in_shared_cache_default = ref false
let diff_command = ref None
let wait_for_filesystem_clock = ref false
let store_orig_src_dir = ref false
let ignore_promoted_rules = ref false
let promote_install_files = ref false
let debug_package_logs = ref false
let concurrency = ref 1

type on_missing_dune_project_file =
  | Error
  | Warn
  | Ignore

let on_missing_dune_project_file = ref Warn
