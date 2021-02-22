(** Dummy scheduler for tests using fibers *)

type t

exception Never

val create : unit -> t

val yield : t -> unit Fiber.t

val run : t -> 'a Fiber.t -> 'a
