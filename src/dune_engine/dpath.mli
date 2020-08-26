open Stdune

module Target_dir : sig
  type context_related =
    | Root
    | With_context of Context_name.t * Path.Source.t

  val build_dir : context_related -> Path.Build.t

  type t =
    | Install of context_related
    | Alias of context_related
    | Regular of context_related
    | Invalid of Path.Build.t

  val of_target : Path.Build.t -> t
end

type target_kind =
  | Regular of Context_name.t * Path.Source.t
  | Alias of Context_name.t * Path.Source.t
  | Install of Context_name.t * Path.Source.t
  | Other of Path.Build.t

type 'build path_kind =
  | Source of Path.Source.t
  | External of Path.External.t
  | Build of 'build

(** Return the name of an alias from its stamp file *)
val analyse_target : Path.Build.t -> target_kind

val analyse_path : Path.t -> target_kind path_kind

val analyse_dir : Path.t -> Target_dir.t path_kind

(** Nice description of a target *)
val describe_target : Path.Build.t -> string

val describe_path : Path.t -> string

include Dune_lang.Conv.S with type t = Path.t

module Local : sig
  val encode : dir:Path.t -> Path.t Dune_lang.Encoder.t

  val decode : dir:Path.t -> Path.t Dune_lang.Decoder.t
end

module Build : sig
  include Dune_lang.Conv.S with type t = Path.Build.t

  val is_dev_null : t -> bool

  val install_dir : t

  val alias_dir : t

  val is_alias_stamp_file : t -> bool
end
