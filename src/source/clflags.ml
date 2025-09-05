type on_missing_dune_project_file =
  | Error
  | Warn
  | Ignore

let on_missing_dune_project_file = ref Warn
