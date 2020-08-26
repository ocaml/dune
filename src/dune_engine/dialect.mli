open! Stdune

(** Dialects

    A dialect is an alternative frontend to OCaml (such as ReasonML). It is
    described by a pair of file extensions, one corresponding to interfaces and
    one to implementations.

    The extensions are unique among all dialects of a given project, so that a
    given extension can be mapped back to the corresponding dialect.

    A dialect can use the standard OCaml syntax or it can specify an action to
    convert from a custom syntax to a binary OCaml syntax.

    Similarly, a dialect can specify a custom formatter to implement the \@fmt
    alias.

    When not using a custom syntax or formatting action, a dialect is nothing
    but a way to specify custom file extensions for OCaml code. *)

type t

val name : t -> string

val to_dyn : t -> Dyn.t

val decode : t Dune_lang.Decoder.t

val extension : t -> Ml_kind.t -> string

val preprocess : t -> Ml_kind.t -> (Loc.t * Action_dune_lang.t) option

val format : t -> Ml_kind.t -> (Loc.t * Action_dune_lang.t * string list) option

val ocaml : t

val reason : t

val ml_suffix : t -> Ml_kind.t -> string option

module DB : sig
  type dialect

  type t

  val empty : t

  val add : t -> loc:Loc.t -> dialect -> t

  val find_by_name : t -> string -> dialect option

  val find_by_extension : t -> string -> (dialect * Ml_kind.t) option

  val to_dyn : t -> Dyn.t

  val builtin : t
end
with type dialect := t
