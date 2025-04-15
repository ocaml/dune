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

module Named_target : sig
  type 'path t =
    | Anonymous of 'path * Kind.t
    | Named of string * ('path * Kind.t)
end

module Static : sig
  type 'path t =
    { targets : ('path * Kind.t) list
    ; multiplicity : Multiplicity.t
    ; named_targets : (string * 'path) list
    }
end

(** [Static] targets are listed by the user while [Infer] denotes that Dune must
    discover all the targets. In the [Static] case, Dune still implicitly adds
    the list of inferred targets. *)
type 'a t =
  | Static of 'a Static.t
  | Infer

(** [target] or [targets] field with the correct multiplicity. *)
val field
  :  allow_directory_targets:bool
  -> string
  -> (String_with_vars.t, Dune_sexp.Decoder.values) Dune_sexp.Decoder.parser
  -> String_with_vars.t t Dune_sexp.Decoder.fields_parser

val decode_target
  :  allow_directory_targets:bool
  -> String_with_vars.t Named_target.t Dune_sexp.Decoder.t

val extract_named_targets : 'path Named_target.t list -> 'path String.Map.t
