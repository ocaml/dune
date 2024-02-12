(** Information about a package defined in the workspace *)

open Import

module Name : sig
  type t = Dune_lang.Package_name.t

  val opam_fn : t -> Filename.t

  include module type of Dune_lang.Package_name with type t := t

  val of_opam_file_basename : string -> t option

  module Map_traversals : sig
    val parallel_iter : 'a Map.t -> f:(t -> 'a -> unit Memo.t) -> unit Memo.t
    val parallel_map : 'a Map.t -> f:(t -> 'a -> 'b Memo.t) -> 'b Map.t Memo.t
  end
end

module Id : sig
  type t

  val name : t -> Name.t

  include Comparable_intf.S with type key := t
end

module Dependency : module type of Dune_pkg.Package_dependency

type opam_file =
  | Exists of bool
  | Generated

type t

val loc : t -> Loc.t
val deprecated_package_names : t -> Loc.t Name.Map.t
val sites : t -> Section.t Site.Map.t
val equal : t -> t -> bool
val name : t -> Name.t
val dir : t -> Path.Source.t
val set_inside_opam_dir : t -> dir:Path.Source.t -> t
val file : dir:Path.t -> name:Name.t -> Path.t
val encode : Name.t -> t Dune_lang.Encoder.t
val decode : dir:Path.Source.t -> t Dune_lang.Decoder.t
val opam_file : t -> Path.Source.t
val to_dyn : t -> Dyn.t
val hash : t -> int
val set_has_opam_file : t -> opam_file -> t
val version : t -> Package_version.t option
val depends : t -> Dependency.t list
val conflicts : t -> Dependency.t list
val depopts : t -> Dependency.t list
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
val map_depends : t -> f:(Dependency.t list -> Dependency.t list) -> t

val create
  :  name:Name.t
  -> dir:Path.Source.t
  -> depends:Dependency.t list
  -> synopsis:string option
  -> description:string option
  -> tags:string list
  -> t

(** Construct a package description from an opam file and its contents *)
val load_opam_file_with_contents : contents:string -> Path.Source.t -> Name.t -> t

val to_local_package : t -> Dune_pkg.Local_package.t
