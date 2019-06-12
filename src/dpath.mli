open Stdune

type target_kind =
  | Regular of string (* build context *) * Path.Source.t
  | Alias   of string (* build context *) * Path.Source.t
  | Install of string (* build context *) * Path.Source.t
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
