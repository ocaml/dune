open! Stdune

module Constraint : sig
  module Op : sig
    type t = Dune_lang.Relop.t

    val to_opam : t -> OpamParserTypes.relop
    val of_opam : OpamParserTypes.relop -> t
  end

  module Value : sig
    type t = Dune_lang.Package_constraint.Value.t
  end

  type t = Dune_lang.Package_constraint.t

  val to_dyn : t -> Dyn.t
end

type t = Dune_lang.Package_dependency.t =
  { name : Package_name.t
  ; constraint_ : Constraint.t option
  }

include module type of Dune_lang.Package_dependency with type t := t

val opam_depend : t -> OpamParserTypes.FullPos.value
val list_to_opam_filtered_formula : t list -> OpamTypes.filtered_formula

(** Attempt to interpret a [OpamTypes.filtered_formula] as a list of [t]s by
    treating the formula as a conjunction of packages with constraints. *)
val list_of_opam_filtered_formula : Loc.t -> OpamTypes.filtered_formula -> t list
