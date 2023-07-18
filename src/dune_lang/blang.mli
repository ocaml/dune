open Stdune
open Dune_sexp

module Op : sig
  type t =
    | Eq
    | Gt
    | Gte
    | Lte
    | Lt
    | Neq

  val eval : t -> Ordering.t -> bool
end

type t =
  | Const of bool
  | Not of t
  | Expr of String_with_vars.t
  | And of t list
  | Or of t list
  | Compare of Op.t * String_with_vars.t * String_with_vars.t

val equal_no_loc : t -> t -> bool
val true_ : t
val to_dyn : t -> Dyn.t
val map_string_with_vars : t -> f:(String_with_vars.t -> String_with_vars.t) -> t
val encode : t Encoder.t
val decode : t Decoder.t

(** Resolve variables manually. For complex cases such as [enabled_if] *)
val decode_manually : (Pform.Env.t -> Template.Pform.t -> Pform.t) -> t Decoder.t
