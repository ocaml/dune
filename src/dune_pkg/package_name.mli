open! Stdune

type t = Dune_lang.Package_name.t

include module type of Dune_lang.Package_name with type t := t

val of_opam_package_name : OpamTypes.name -> t
val to_opam_package_name : t -> OpamTypes.name
