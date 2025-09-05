open Import
open Ocaml

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

module Format : sig
  type t =
    | Action of (Loc.t * Action.t)
    | Ocamlformat
end

val name : t -> string
val to_dyn : t -> Dyn.t
val encode : t Encoder.t
val decode : t Decoder.t
val extension : t -> Ml_kind.t -> Filename.Extension.t option
val preprocess : t -> Ml_kind.t -> (Loc.t * Action.t) option
val format : t -> Ml_kind.t -> Format.t option
val print_ast : t -> Ml_kind.t -> (Loc.t * Action.t) option
val ocaml : t
val reason : t
val rescript : t
val ml_suffix : t -> Ml_kind.t -> string option

module DB : sig
  type dialect := t
  type t

  val empty : t
  val add : t -> loc:Loc.t -> dialect -> t
  val find_by_name : t -> string -> dialect option
  val find_by_extension : t -> Filename.Extension.t -> (dialect * Ml_kind.t) option
  val fold : t -> init:'a -> f:(dialect -> 'a -> 'a) -> 'a
  val to_dyn : t -> Dyn.t
  val builtin : t
  val is_default : t -> bool

  type for_merlin =
    { extensions : string option Ml_kind.Dict.t list
    ; readers : Filename.Extension.t list String.Map.t
    }

  val for_merlin : t -> for_merlin
end
