open Import

module Target_dir : sig
  type context_related =
    | Root
    | With_context of Context_name.t * Path.Source.t

  val build_dir : context_related -> Path.Build.t

  type t =
    | Anonymous_action of context_related
    | Regular of context_related
    | Invalid of Path.Build.t

  val of_target : Path.Build.t -> t
end

type target_kind =
  | Regular of Context_name.t * Path.Source.t
  | Alias of Context_name.t * Path.Source.t
  | Anonymous_action of Context_name.t
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

type t = Path.t

val encode : Path.t Dune_sexp.Encoder.t

module Build : sig
  type t = Path.Build.t

  val anonymous_actions_dir : t
  val anonymous_actions_dir_basename : Filename.t
end
