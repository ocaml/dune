open Stdune

type t = private
  | Env of Env.Var.t
  | File of Path.t
  | Glob of Path.t * string Predicate.t
  | Universe

val file : Path.t -> t
val env : Env.Var.t -> t
val universe : t
val glob : dir:Path.t -> string Predicate.t -> t

val compare : t -> t -> Ordering.t

val pp : t Fmt.t

module Set : sig
  include Set.S with type elt = t

  val has_universe : t -> bool

  val paths
    :  t
    -> eval_pred:(dir:Path.t -> string Predicate.t -> string list)
    -> Path.Set.t

  val encode : t -> Dune_lang.t

  val trace
    :  t
    -> env:Env.t
    -> eval_pred:(dir:Path.t -> string Predicate.t -> string list)
    -> (string * Digest.t) list

  val add_paths : t -> Path.Set.t -> t

  val parallel_iter
    : t -> f:(elt -> unit Fiber.t)
    -> unit Fiber.t

  val parallel_iter_files
    : t
    -> f:(Path.t -> unit Fiber.t)
    -> eval_pred:(dir:Path.t -> string Predicate.t -> string list)
    -> unit Fiber.t

  val pp : t Fmt.t

  val dirs : t -> Path.Set.t
end
