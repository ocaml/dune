open Import

(** Block strings as represented in the concrete syntax tree. *)

module Kind : sig
  type t =
    | Escaped
    | Raw
end

(** The kind and template parts of each logical line. *)
type t = (Kind.t * Template.Part.t list) list

val repr : t Repr.t

(** Convert a block string to its abstract representation. *)
val to_ast : loc:Loc.t -> t -> Ast.t
