open! Import

type 'a t =
  | A        of string
  | As       of string list
  | S        of 'a t list
  | Concat   of string * 'a t list (** Concatenation with a custom separator *)
  | Dep      of Path.t (** A path that is a dependency *)
  | Deps     of Path.t list
  | Target   of Path.t
  | Path     of Path.t
  | Paths    of Path.t list
  | Dyn      of ('a -> nothing t)

val add_deps    : _ t list -> Path.Set.t -> Path.Set.t
val add_targets : _ t list -> Path.t list -> Path.t list
val expand      : dir:Path.t -> 'a t list -> 'a -> string list * Path.Set.t

(** [quote_args quote args] is [As \[quote; arg1; quote; arg2; ...\]] *)
val quote_args : string -> string list -> _ t
