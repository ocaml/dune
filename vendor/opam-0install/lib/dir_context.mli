(** A context that reads opam files directly from an opam-repository checkout.
    It does not use a switch, and therefore does not pick up any pins the user has set.
    It also does not get any opam variables from the environment - instead, the caller
    must provide them explicitly. *)

module Make (Monad : S.Monad) : sig

include S.CONTEXT with type 'a monad := 'a Monad.t

val std_env :
  ?ocaml_native:bool ->
  ?sys_ocaml_version:string ->
  ?opam_version:string ->
  arch:string ->
  os:string ->
  os_distribution:string ->
  os_family:string ->
  os_version:string ->
  unit ->
  (string -> OpamVariable.variable_contents option)
(** [std_env ~arch ~os ~os_distribution ~os_family ~os_version] is an
    environment function that returns the given values for the standard opam
    variables, and [None] for anything else.
    If [opam_version] is not provided, use the version of the linked opam
    library. *)

val create :
  ?prefer_oldest:bool ->
  ?test:OpamPackage.Name.Set.t ->
  ?pins:(OpamTypes.version * OpamFile.OPAM.t) OpamTypes.name_map ->
  constraints:OpamFormula.version_constraint OpamTypes.name_map ->
  env:(string -> OpamVariable.variable_contents option) ->
  string ->
  t
(** [create ~constraints ~env packages_dir] is a solver that gets candidates
    from [packages_dir], filtering them using [constraints]. [packages_dir] contains
    one sub-directory for each package name, each with subdirectories for each version, in
    the same format used by opam-repository.
    @param test Packages for which we should include "with-test" dependencies.
    @param pins Packages in this map have only the given candidate version and opam file.
    @param env Maps opam variable names to values ({!std_env} may be useful here).
               "version" and the [OpamPackageVar.predefined_depends_variables] are handled automatically.
    @param prefer_oldest if [true] the solver is set to return the least
    up-to-date version of each package, if a solution exists. This is [false] by
    default.
    @before 0.4 the [prefer_oldest] parameter did not exist. *)
end
