(** Information about a package defined in the workspace *)

open Import

module Name : sig
  type t = Dune_lang.Package_name.t

  val opam_fn : t -> string

  val version_fn : t -> string

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

module Dependency : sig
  module Op : sig
    type t =
      | Eq
      | Gte
      | Lte
      | Gt
      | Lt
      | Neq

    val to_relop : t -> OpamParserTypes.FullPos.relop
  end

  module Constraint : sig
    module Var : sig
      type t =
        | Literal of string
            (** A quoted string literal, such as a version number *)
        | Var of string  (** A variable name such as :version or :with-test *)
    end

    type t =
      | Bvar of Var.t
      | Uop of Op.t * Var.t
      | Bop of Op.t * Var.t * Var.t
      | And of t list
      | Or of t list

    val to_dyn : t -> Dyn.t
  end

  type t =
    { name : Name.t
    ; constraint_ : Constraint.t option
    }

  val opam_depend : t -> OpamParserTypes.FullPos.value

  val to_dyn : t -> Dyn.t

  val decode : t Dune_lang.Decoder.t
end

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

  val decode :
       ?since:Dune_lang.Syntax.Version.t
    -> unit
    -> t Dune_lang.Decoder.fields_parser

  val superpose : t -> t -> t
end

type opam_file =
  | Exists of bool
  | Generated

type t =
  { id : Id.t
  ; opam_file : Path.Source.t
  ; loc : Loc.t
  ; synopsis : string option
  ; description : string option
  ; depends : Dependency.t list
  ; conflicts : Dependency.t list
  ; depopts : Dependency.t list
  ; info : Info.t
  ; version : string option
  ; has_opam_file : opam_file
  ; tags : string list
  ; deprecated_package_names : Loc.t Name.Map.t
  ; sites : Section.t Section.Site.Map.t
  ; allow_empty : bool
  }

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

(** Construct a default package (e.g., for project initialization) *)
val default : Name.t -> Path.Source.t -> t

(** Construct a package description from an opam file. *)
val load_opam_file : Path.Source.t -> Name.t -> t Memo.t

val missing_deps : t -> effective_deps:Name.Set.t -> Name.Set.t

(** [to_opam_file t] returns an [OpamFile.OPAM.t] whose fields are based on the
    fields of [t]. Note that this does not actually create a corresponding file
    on disk. *)
val to_opam_file : t -> OpamFile.OPAM.t
