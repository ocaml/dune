(** An event bus is a first-in-first-out queue.

    It has the following invariants:
    - A [push] only finishes when there is a [pop].
    - A [pop] only finishes when there is a [push].
    - Multiple [push]es or [pop]s at once will be blocked until they can be resolved.
      However the order is still preserved.

    It can be [close]d preventing further [push]es and [pop]s. *)
type 'a t

(** [create ()] initializes an empty event bus. *)
val create : unit -> 'a t

(** [push t a] attempts to push a value [a] to the end of an event bus [t]. It returns a
    fiber with [`Ok] if it was successful and [`Closed] otherwise if it was closed. If
    the bus doesn't have another [pop] then it will block.*)
val push : 'a t -> 'a -> [ `Closed | `Ok ] Fiber.t

(** [pop t] attempts to pop the first value from the event bus [t]. It returns a fiber
    with [`Next a] if it was successful and [`Closed] otherwise if it was closed. If the
    bus doesn't have another [push] then it will block. *)
val pop : 'a t -> [ `Closed | `Next of 'a ] Fiber.t

(** [close t] closes the event bus [t] preventing further [push]es and [pop]s. *)
val close : 'a t -> unit Fiber.t
