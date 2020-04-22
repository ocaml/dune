module Promote = struct
  type t =
    | Automatically
    | Never
end

let debug_findlib = ref false

let debug_dep_path = ref false

let external_lib_deps_hint = ref []

let external_lib_deps_mode = ref false

let capture_outputs = ref true

let debug_backtraces = Dune_util.Report_error.report_backtraces

let diff_command = ref None

let promote = ref None

let force = ref false

let watch = ref false

let no_print_directory = ref false

let store_orig_src_dir = ref false

let always_show_command_line = ref false

let promote_install_files = ref false

let ignore_promoted_rules = ref false
