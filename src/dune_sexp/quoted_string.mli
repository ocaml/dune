module Block_kind : sig
  type t =
    | Escaped
    | Raw

  val delimiter : t -> string
  val to_dyn : t -> Dyn.t
  val equal : t -> t -> bool
end

type t =
  | Single of string
  | Multi of (Block_kind.t * Part.t list) list

val to_string : t -> string
val equal : t -> t -> bool
val to_dyn : t -> Dyn.t

(** Result of flattening a quoted string for AST conversion *)
type flattened =
  | String of string (** No pforms - can be a plain Quoted_string *)
  | Parts of Part.t list (** Has pforms - must be a Template *)

(** Flatten a quoted string for conversion to AST. Multi-line block strings
    are flattened with newlines between lines. If any pforms are present,
    returns [Parts] so they can be preserved in a Template. *)
val flatten : t -> flattened
