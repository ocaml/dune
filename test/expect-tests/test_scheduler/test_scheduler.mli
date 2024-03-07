(** Dummy scheduler for tests using fibers *)

type t

exception Never

val create : unit -> t
val yield : t -> unit Fiber.t
val yield_gen : t -> do_in_scheduler:(unit -> 'a) -> 'a Fiber.t
val run : t -> 'a Fiber.t -> 'a
