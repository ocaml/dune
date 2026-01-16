(** Conceptually, a [unit Fiber.Ivar.t] that can be filled idempotently.
    Ivars cannot, in general, have idempotent fill operations, since the second
    fill might have different data in it than the first. Since this type only
    contains unit data, we can be sure that every fill will be (). In fact, we
    don't even both accepting data as the parameter of [trigger], since we
    already know what it must be. *)
type t

val create : unit -> t
val trigger : t -> Fiber.fill list
val wait : t -> unit Fiber.t
