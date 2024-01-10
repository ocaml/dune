open! Stdune

(** Note that this type is not equivalent to [Dune_lang.Package_name.t] to
    prevent opam-incompatible package names from being represented in
    package-management code. *)
type t = Dune_lang.Package_name.Opam_compatible.t

include module type of Dune_lang.Package_name.Opam_compatible with type t := t

val of_opam_package_name : OpamTypes.name -> t
val to_opam_package_name : t -> OpamTypes.name
