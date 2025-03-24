(** Information about a package defined in the workspace *)

open Stdune

module Name : sig
  type t = Package_name.t

  include module type of Package_name with type t := t
end

module Id : sig
  type t

  val name : t -> Name.t

  include Comparable_intf.S with type key := t
end

type opam_file =
  | Exists of bool
  | Generated

type t

val loc : t -> Loc.t
val deprecated_package_names : t -> Loc.t Name.Map.t
val sites : t -> Section.t Site.Map.t
val name : t -> Name.t
val dir : t -> Path.Source.t
val set_inside_opam_dir : t -> dir:Path.Source.t -> t
val encode : Name.t -> t Dune_sexp.Encoder.t
val decode : dir:Path.Source.t -> t Dune_sexp.Decoder.t
val opam_file : t -> Path.Source.t
val to_dyn : t -> Dyn.t
val hash : t -> int
val set_has_opam_file : t -> opam_file -> t
val version : t -> Package_version.t option
val depends : t -> Package_dependency.t list
val conflicts : t -> Package_dependency.t list
val depopts : t -> Package_dependency.t list
val tags : t -> string list
val synopsis : t -> string option
val info : t -> Package_info.t
val description : t -> string option
val id : t -> Id.t

val set_version_and_info
  :  t
  -> version:Package_version.t option
  -> info:Package_info.t
  -> t

val has_opam_file : t -> opam_file
val allow_empty : t -> bool
val map_depends : t -> f:(Package_dependency.t list -> Package_dependency.t list) -> t

type original_opam_file =
  { file : Path.Source.t
  ; contents : string
  }

val create
  :  name:Name.t
  -> loc:Loc.t
  -> version:Package_version.t option
  -> conflicts:Package_dependency.t list
  -> depends:Package_dependency.t list
  -> depopts:Package_dependency.t list
  -> info:Package_info.t
  -> has_opam_file:opam_file
  -> dir:Path.Source.t
  -> sites:Section.t Site.Map.t
  -> allow_empty:bool
  -> synopsis:string option
  -> description:string option
  -> tags:string list
  -> original_opam_file:original_opam_file option
  -> deprecated_package_names:Loc.t Name.Map.t
  -> t

val original_opam_file : t -> original_opam_file option
