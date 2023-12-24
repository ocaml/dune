(** Information about a package defined in the workspace *)

open Import

module Name : sig
  type t = Dune_lang.Package_name.t

  val opam_fn : t -> Filename.t
  val version_fn : t -> Filename.t

  include module type of Dune_lang.Package_name with type t := t

  val of_opam_file_basename : string -> t option
  val of_opam_package_name : OpamTypes.name -> t
  val to_opam_package_name : t -> OpamTypes.name

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

module Source_kind : sig
  module Host : sig
    type kind =
      | Github
      | Bitbucket
      | Gitlab
      | Sourcehut

    type t =
      { user : string
      ; repo : string
      ; kind : kind
      }

    val homepage : t -> string
  end

  type t =
    | Host of Host.t
    | Url of string

  val to_dyn : t Dyn.builder
  val to_string : t -> string
  val decode : t Dune_lang.Decoder.t
end

module Info : sig
  type t

  val source : t -> Source_kind.t option
  val license : t -> string list option
  val authors : t -> string list option
  val homepage : t -> string option
  val bug_reports : t -> string option
  val documentation : t -> string option
  val maintainers : t -> string list option

  (** example package info (used for project initialization ) *)
  val example : t

  val empty : t
  val to_dyn : t Dyn.builder
  val encode_fields : t -> Dune_lang.t list

  val decode
    :  ?since:Dune_lang.Syntax.Version.t
    -> unit
    -> t Dune_lang.Decoder.fields_parser

  val superpose : t -> t -> t
end

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
val meta_file : t -> Path.Source.t
val deprecated_meta_file : t -> Name.t -> Path.Source.t
val to_dyn : t -> Dyn.t
val hash : t -> int
val is_opam_file : Path.t -> bool
val set_has_opam_file : t -> opam_file -> t
val version : t -> Package_version.t option
val depends : t -> Dependency.t list
val conflicts : t -> Dependency.t list
val depopts : t -> Dependency.t list
val tags : t -> string list
val synopsis : t -> string option
val info : t -> Info.t
val description : t -> string option
val id : t -> Id.t
val set_version_and_info : t -> version:Package_version.t option -> info:Info.t -> t
val has_opam_file : t -> opam_file
val allow_empty : t -> bool
val map_depends : t -> f:(Dependency.t list -> Dependency.t list) -> t

(** Construct a default package (e.g., for project initialization) *)
val default : Name.t -> Path.Source.t -> t

(** Construct a package description from an opam file. *)
val load_opam_file : Path.Source.t -> Name.t -> t Memo.t

val missing_deps : t -> effective_deps:Name.Set.t -> Name.Set.t
val to_local_package : t -> Dune_pkg.Local_package.t
