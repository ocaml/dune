open Stdune

type target_kind =
  | Regular of Context_name.t * Path.Source.t
  | Alias of Context_name.t * Path.Source.t
  | Install of Context_name.t * Path.Source.t
  | Other of Path.Build.t

type path_kind =
  | Source of Path.Source.t
  | External of Path.External.t
  | Build of target_kind

(** Return the name of an alias from its stamp file *)
val analyse_target : Path.Build.t -> target_kind

val analyse_path : Path.t -> path_kind

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
