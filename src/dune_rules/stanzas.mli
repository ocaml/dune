open Import

type t =
  { ctx_dir : Path.Build.t
  ; src_dir : Path.Source.t
  ; project : Dune_project.t
  ; stanzas : Stanza.t list
  }

val in_dir : Path.Build.t -> t option Memo.t
