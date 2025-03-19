open Import

type t =
  { coq_flags : string list
  ; coqdep_flags : string list
  ; coqdoc_flags : string list
  ; coqdoc_header : Path.t option
  ; coqdoc_footer : Path.t option
  }

val default : t
val dump : dir:Path.t -> t -> Dune_lang.t list
