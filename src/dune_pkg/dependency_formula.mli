type t

val of_dependencies : Package_dependency.t list -> t
val to_filtered_formula : t -> OpamTypes.filtered_formula
val of_filtered_formula : OpamTypes.filtered_formula -> t
val to_sexp : t -> Dune_sexp.t
val union : t list -> t
val remove_packages : t -> Package_name.Set.t -> t
val has_entries : t -> bool
val any_package_name : t -> Package_name.t option
