open! Stdune

type t = Dune_lang.Package_version.t

val of_string : string -> t
val to_string : t -> string
val equal : t -> t -> bool
val to_dyn : t -> Dyn.t
val encode : t Dune_lang.Encoder.t
val decode : t Dune_lang.Decoder.t
val of_opam_package_version : OpamPackage.Version.t -> t
val to_opam_package_version : t -> OpamPackage.Version.t
val dev : t
