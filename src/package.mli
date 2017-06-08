(** Information about a package defined in the workspace *)

type t =
  { name                   : string
  ; path                   : Path.t
  ; version_from_opam_file : string option
  }

val opam_file : t -> Path.t
