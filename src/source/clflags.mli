type on_missing_dune_project_file =
  | Error
  | Warn
  | Ignore

(** Desired behavior when dune project file is absent *)
val on_missing_dune_project_file : on_missing_dune_project_file ref
