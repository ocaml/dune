open! Stdune

module Constraint : sig
  module Op : sig
    type t = Dune_lang.Package_constraint.Op.t

    val to_relop : t -> OpamParserTypes.FullPos.relop
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
