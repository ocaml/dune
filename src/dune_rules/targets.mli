(** Defines target behavior for rules. *)
open! Dune_engine

open Import

module Multiplicity : sig
  type t =
    | One  (** [target] field or variable *)
    | Multiple  (** [targets] field or variable *)

  val check_variable_matches_field : loc:Loc.t -> field:t -> variable:t -> unit
end

module Static : sig
  type 'path t =
    { targets : 'path list
    ; multiplicity : Multiplicity.t
    }
end

(** Static targets are listed by the user while [Infer] denotes that dune must
    discover all the targets. In the [Static] case, dune still implicitly adds
    the list of inferred targets *)
type 'a t =
  | Static of 'a Static.t
  | Infer

module Or_forbidden : sig
  (** In some situations, actions may not have targets. [Forbidden _] is used to
      denote that *)
  type nonrec t =
    | Forbidden of string
    | Targets of Path.Build.t t
end

(** target or targets with field with the correct multiplicity *)
val field : String_with_vars.t t Dune_lang.Decoder.fields_parser
