(** Specification of targets. *)
open! Dune_engine

open Import

module Multiplicity : sig
  type t =
    | One  (** [target] field or variable *)
    | Multiple  (** [targets] field or variable *)

  val check_variable_matches_field : loc:Loc.t -> field:t -> variable:t -> unit
end

(** Tags are used to distinguish file and directory targets. Specifically, a
    directory target is specified by adding "/*" at the end. *)
module Tag : sig
  type t =
    | None
    | Star  (** Ends with "/*", i.e. "output/*" *)
end

module Static : sig
  type 'path t =
    { targets : 'path list  (** Here ['path] may be tagged with [Tag.t]. *)
    ; multiplicity : Multiplicity.t
    }
end

(** Static targets are listed by the user while [Infer] denotes that dune must
    discover all the targets. In the [Static] case, dune still implicitly adds
    the list of inferred targets. *)
type 'a t =
  | Static of 'a Static.t
  | Infer

(** [target] or [targets] field with the correct multiplicity. *)
val field : String_with_vars.t t Dune_lang.Decoder.fields_parser

(** Contains a directory target. *)
val has_target_directory : String_with_vars.t t -> bool

val untag : ('a * Tag.t) t -> 'a t
