val name : Package_name.t
val version : OpamPackage.Version.t
val package : OpamPackage.t
val opam_file : OpamFile.OPAM.t

(** Strip upper version bounds ([<], [<=]) on dune atoms from a depends
    formula. Filter sub-atoms and lower-bound / equality constraints are
    preserved. Opam upper bounds on dune are typically defensive and grow
    stale with dune's strong backward compatibility; lower bounds still
    convey a meaningful minimum-dune signal. *)
val strip_upper_version_constraints
  :  OpamTypes.filtered_formula
  -> OpamTypes.filtered_formula
