open! Import

(** Information about a package that's relevant for solving dependencies *)
type t =
  { name : Package_name.t
  ; version : Package_version.t option
  ; dependencies : Package_dependency.t list
  ; loc : Loc.t
  }

(** [to_opam_file t] returns an [OpamFile.OPAM.t] whose fields are based on the
    fields of [t]. Note that this does not actually create a corresponding file
    on disk. *)
val to_opam_file : t -> OpamFile.OPAM.t

(** Returns an opam dependency formula for this package *)
val opam_filtered_dependency_formula : t -> OpamTypes.filtered_formula
