let store_orig_src_dir = ref false
let ignore_promoted_rules = ref false
let promote_install_files = ref false
let display = Dune_engine.Clflags.display
let capture_outputs = Dune_engine.Clflags.capture_outputs
let debug_artifact_substitution = ref false
let ignore_lock_dir = ref false

type on_missing_dune_project_file =
  | Error
  | Warn
  | Ignore

let on_missing_dune_project_file = ref Warn
