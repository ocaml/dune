open! Import

val package_name : OpamTypes.name -> Dyn.t
val package : OpamPackage.t -> Dyn.t
val relop : OpamTypes.relop -> Dyn.t
val variable : OpamTypes.variable -> Dyn.t
val filter : OpamTypes.filter -> Dyn.t
val formula : ('a -> Dyn.t) -> 'a OpamFormula.formula -> Dyn.t
val filtered_formula : OpamTypes.filtered_formula -> Dyn.t
