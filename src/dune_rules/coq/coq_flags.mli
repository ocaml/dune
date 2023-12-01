type t =
  { coq_flags : string list
  ; coqdoc_flags : string list
  }

val default : t
val dump : t -> Dune_lang.t list
