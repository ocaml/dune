type t

val of_dependencies : Package_dependency.t list -> t
val to_filtered_formula : t -> OpamTypes.filtered_formula
val of_filtered_formula : OpamTypes.filtered_formula -> t
