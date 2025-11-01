(** Returns the version from the current project's .ocamlformat file,
    if it exists *)
val version_of_current_project's_ocamlformat_config : unit -> Package_version.t option
