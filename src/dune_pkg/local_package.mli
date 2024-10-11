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
  ; dependencies : Dependency_formula.t
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

module For_solver : sig
  (** The minimum set of fields about a package needed by the solver. *)
  type t =
    { name : Package_name.t
    ; dependencies : Dependency_formula.t
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

  (** Returns a hash of the dependencies that are not local to the project (if any) *)
  val non_local_dependency_hash : t list -> Dependency_hash.t option

  (** Returns the name of the first non-local dependency it can find *)
  val any_non_local_dependency_name : t list -> Package_name.t option

  (** Returns the names of packages that it depends on *)
  val dependency_names : t -> Package_name.t list
end

val for_solver : t -> For_solver.t
val of_package : Dune_lang.Package.t -> t
