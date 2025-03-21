open Stdune

type t

module Name : sig
  type t = Package_name.t

  include module type of Package_name with type t := t
end

module Opam_package : sig
  type t

  val create
    :  name:Name.t
    -> loc:Loc.t
    -> version:Package_version.t option
    -> conflicts:Package_dependency.t list
    -> depends:OpamTypes.filtered_formula
    -> depopts:Package_dependency.t list
    -> info:Package_info.t
    -> dir:Path.Source.t
    -> synopsis:string option
    -> description:string option
    -> tags:string list
    -> t

  val depends : t -> OpamTypes.filtered_formula
  val depopts : t -> Package_dependency.t list
  val conflicts : t -> Package_dependency.t list
end

val of_dune_package : Dune_package.t -> t
val of_opam_package : Opam_package.t -> t
val name : t -> Package_name.t
val info : t -> Package_info.t
val version : t -> Package_version.t option
val opam_file : t -> Stdune.Path.Source.t
val tags : t -> string list
val sites : t -> Section.t Site.Map.t
val deprecated_package_names : t -> Loc.t Name.Map.t
val loc : t -> Loc.t
val dir : t -> Path.Source.t
val synopsis : t -> string option
val description : t -> string option
val dune_package : t -> Dune_package.t option
val opam_package : t -> Opam_package.t option
val to_dyn : t -> Dyn.t

val set_version_and_info
  :  t
  -> version:Package_version.t option
  -> info:Package_info.t
  -> t

val id : t -> Package_id.t
val allow_empty : t -> bool
