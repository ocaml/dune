open! Stdune

module Filter : sig
  type t =
    | No_filter
    | Action of Loc.t * Action_dune_lang.t

  val to_dyn : t -> Dyn.t
end

type t

val name : t -> string

val to_dyn : t -> Dyn.t

val extension : t -> Ml_kind.t -> string

val preprocess : t -> Ml_kind.t -> Filter.t

val format : t -> Ml_kind.t -> Filter.t

val ocaml : t

val reason : t

val ml_suffix : t -> Ml_kind.t -> string option
