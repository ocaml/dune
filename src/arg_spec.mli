open! Import

type 'a t =
  | A        of string
  | As       of string list
  | S        of 'a t list
  | Dep      of Path.t (** A path that is a dependency *)
  | Deps     of Path.t list
  | Dep_rel  of Path.t * string
  | Deps_rel of Path.t * string list
  | Path     of Path.t
  | Paths    of Path.t list
  | Dyn      of ('a -> nothing t)

val add_deps : _ t list -> Path.Set.t -> Path.Set.t
val expand   : dir:Path.t -> 'a t list -> 'a -> string list * Path.Set.t

