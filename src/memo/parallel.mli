open! Import

(** Parallel combinators that, while a Memo computation is running, record the dependencies
    discovered by independent branches as a parallel section (so that restoring from the
    cache can check them concurrently). With no running computation they behave exactly like
    the corresponding [Fiber] combinators. *)

val fork_and_join : (unit -> 'a Fiber.t) -> (unit -> 'b Fiber.t) -> ('a * 'b) Fiber.t
val fork_and_join_unit : (unit -> unit Fiber.t) -> (unit -> 'a Fiber.t) -> 'a Fiber.t
val all_concurrently : 'a Fiber.t list -> 'a list Fiber.t
val all_concurrently_unit : unit Fiber.t list -> unit Fiber.t
val parallel_map : 'a list -> f:('a -> 'b Fiber.t) -> 'b list Fiber.t
val parallel_iter : 'a list -> f:('a -> unit Fiber.t) -> unit Fiber.t

val parallel_iter_set
  :  (module Set.S with type elt = 'a and type t = 's)
  -> 's
  -> f:('a -> unit Fiber.t)
  -> unit Fiber.t

val map_reduce
  :  'a list
  -> f:('a -> 'b Fiber.t)
  -> empty:'b
  -> combine:('b -> 'b -> 'b)
  -> 'b Fiber.t

val map_reduce_array
  :  'a array
  -> f:('a -> 'b Fiber.t)
  -> empty:'b
  -> combine:('b -> 'b -> 'b)
  -> 'b Fiber.t
