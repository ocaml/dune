open Stdune

type t

val file : Path.t -> t
val env : Env.Var.t -> t

val compare : t -> t -> Ordering.t

val pp : t Fmt.t

module Set : sig
  include Set.S with type elt = t

  val paths : t -> Path.Set.t

  val encode : t -> Dune_lang.t

  val trace : t -> env:Env.t -> (string * Stdune.Digest.t) list

  val add_paths : t -> Path.Set.t -> t

  val parallel_iter : t -> f:(Path.t -> unit Fiber.t) -> unit Fiber.t
  val pp : t Fmt.t
end
