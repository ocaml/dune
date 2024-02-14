open! Import

type pin =
  { loc : Loc.t
  ; version : Package_version.t
  ; url : Loc.t * OpamUrl.t
  ; name : Package_name.t
  ; origin : [ `Dune | `Opam ]
  }

type pins = pin Package_name.Map.t

(** Information about a local package that's relevant for package management.
    This is intended to represent local packages defined in a dune-project file
    (rather than packages in a lockdir). This is distinct from a
    [Dune_rules.Package.t] in that it's only intended to represent local
    packages. The "dune_pkg" library can't depend on "dune_rules" to avoid a
    circular dependency so this type is defined here so "dune_pkg" has a type
    it can use to represent local packages. *)
type t =
  { name : Package_name.t
  ; version : Package_version.t option
  ; dependencies : Package_dependency.t list
  ; conflicts : Package_dependency.t list
  ; conflict_class : Package_name.t list
  ; depopts : Package_dependency.t list
  ; pins : pins
  ; loc : Loc.t
  }

module Dependency_hash : sig
  type t

  val equal : t -> t -> bool
  val to_dyn : t -> Dyn.t
  val to_string : t -> string
  val encode : t Encoder.t
  val decode : t Decoder.t
end

module Dependency_set : sig
  (** A set of dependencies belonging to one or more local packages. Two
      different local packages may depend on the same packages with different
      version constraints provided that the two constraints intersect
      (otherwise there will be no solution). In this case the conjunction of
      both constraints will form the constraint associated with that
      dependency. Package constraints are de-duplicated by comparing them only
      on their syntax. *)
  type t

  (** Returns a hash of all dependencies in the set or [None] if the set is
      empty. The reason for behaving differently when the set is empty is so
      that callers are forced to explicitly handle the case where there are no
      dependencies which will likely lead to better user experience. *)
  val hash : t -> Dependency_hash.t option

  val package_dependencies : t -> Package_dependency.t list
end

module For_solver : sig
  (** The minimum set of fields about a package needed by the solver. *)
  type t =
    { name : Package_name.t
    ; dependencies : Package_dependency.t list
    ; conflicts : Package_dependency.t list
    ; depopts : Package_dependency.t list
    ; conflict_class : Package_name.t list
    ; pins : pins
    }

  (** [to_opam_file t] returns an [OpamFile.OPAM.t] whose fields are based on the
      fields of [t]. Note that this does not actually create a corresponding file
      on disk. *)
  val to_opam_file : t -> OpamFile.OPAM.t

  (** Returns an opam dependency formula for this package *)
  val opam_filtered_dependency_formula : t -> OpamTypes.filtered_formula

  (** Returns the set of dependencies of all given local packages excluding
      dependencies which are packages in the provided list. Pass this the list
      of all local package in a project to get a set of all non-local
      dependencies of the project. *)
  val list_non_local_dependency_set : t list -> Dependency_set.t
end

val for_solver : t -> For_solver.t
val of_package : Dune_lang.Package.t -> t
