(** Specification of targets. *)

open Stdune

module Multiplicity : sig
  type t =
    | One (** [target] field or variable *)
    | Multiple (** [targets] field or variable *)

  val check_variable_matches_field : loc:Loc.t -> field:t -> variable:t -> unit
end

module Kind : sig
  type t =
    | File
    | Directory
end

module Static : sig
  type 'path t =
    { targets : ('path * Kind.t * string option) list
    ; multiplicity : Multiplicity.t
    }
end

(** [Static] targets are listed by the user while [Infer] denotes that Dune must
    discover all the targets. In the [Static] case, Dune still implicitly adds
    the list of inferred targets. *)
type 'a t =
  | Static of 'a Static.t
  | Infer
  | Bindings of 'a Bindings.t

(** [target] or [targets] field with the correct multiplicity. *)
val field
  :  allow_directory_targets:bool
  -> String_with_vars.t t Dune_sexp.Decoder.fields_parser

(* In targets_spec.mli *)
val get_target_by_name : string -> String_with_vars.t t -> String_with_vars.t option

val expand_bindings
  :  String_with_vars.t Bindings.t
  -> (String_with_vars.t * Kind.t * string option) list
