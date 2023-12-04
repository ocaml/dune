open Import

(** Environment for Coq. *)
type t

val equal : t -> t -> bool

(** Default environment for Coq. *)
val default : t

(** Flags for Coq binaries. *)
val flags : t -> Ordered_set_lang.Unexpanded.t

(** Flags for coqdoc *)
val coqdoc_flags : t -> Ordered_set_lang.Unexpanded.t

(** Parser for env stanza. *)
val decode : t Dune_lang.Decoder.fields_parser
