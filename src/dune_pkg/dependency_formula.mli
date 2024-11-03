type t

(** Create a dependency formula out of a [Package_dependency.t] list where
    all packages are dependencies *)
val of_dependencies : Package_dependency.t list -> t

(** Convert to the OPAM data type *)
val to_filtered_formula : t -> OpamTypes.filtered_formula

(** Convert from the OPAM data type to this *)
val of_filtered_formula : OpamTypes.filtered_formula -> t

(** Create a Dyn representation of the dependency formula *)
val to_dyn : t -> Dyn.t

(* Join all dependencies in the list to a single conjunction *)
val ands : t list -> t

(** Remove a package from the entire formula *)
val remove_packages : t -> Package_name.Set.t -> t

(** Determine whether the dependency formula has any dependencies *)
val has_entries : t -> bool

(** Returns the [Package_name.t] of a dependency from the formula, if it
    exists. *)
val any_package_name : t -> Package_name.t option
