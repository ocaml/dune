type t

(** Create a dependency formula out of a [Package_dependency.t] list where
    all packages are dependencies *)
val of_dependencies : Package_dependency.t list -> t

(** Convert to the OPAM data type *)
val to_filtered_formula : t -> OpamTypes.filtered_formula

(** Convert from the OPAM data type to this *)
val of_filtered_formula : OpamTypes.filtered_formula -> t

(** Create an s-exp representation of the dependency formulat *)
val to_sexp : t -> Dune_sexp.t

(* Join two dependency formulas into one that represents a dependency on
   both LHS and RHS *)
val union : t list -> t

(** Remove a package from the entire formula *)
val remove_packages : t -> Package_name.Set.t -> t

(** Determine whether the dependency formula has any dependencies *)
val has_entries : t -> bool

(** Returns the [Package_name.t] of a dependency from the formula, if it
    exists. *)
val any_package_name : t -> Package_name.t option

(** Returns all dependency names that can be found in the formula, no matter
    whether they can be satisfied or not *)
val reachable_dependencies : t -> Package_name.Set.t
