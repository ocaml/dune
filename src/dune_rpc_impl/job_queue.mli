(** Primitive unbounded FIFO channel. Reads are blocking. Writes are not
    blocking. At most one read is allowed at a time. *)

type 'a t

(** Remove the element from the internal queue without waiting for the next
    element. *)
val pop_internal : 'a t -> 'a option

val create : unit -> 'a t
val read : 'a t -> 'a Fiber.t
val write : 'a t -> 'a -> unit Fiber.t
