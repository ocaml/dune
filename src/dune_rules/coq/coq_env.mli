open Import

(** Environment for Coq. *)
type t

val equal : t -> t -> bool

(** Default environment for Coq. *)
val default : t

(** Flags for Coq binaries. *)
val flags : t -> Ordered_set_lang.Unexpanded.t

(** Flags for coqdep *)
val coqdep_flags : t -> Ordered_set_lang.Unexpanded.t

(** Flags for coqdoc *)
val coqdoc_flags : t -> Ordered_set_lang.Unexpanded.t

(** Coqdoc header config. *)
val coqdoc_header : t -> String_with_vars.t option

(** Coqdoc footer config. *)
val coqdoc_footer : t -> String_with_vars.t option

(** Parser for env stanza. *)
val decode : t Dune_lang.Decoder.fields_parser
