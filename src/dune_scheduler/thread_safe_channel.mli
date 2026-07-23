(** A FIFO channel that can be written to from any thread and read from
    fibers running in the scheduler that created it. *)
type 'a t

val create : Event.Queue.t -> 'a t

(** [write t x] makes [x] available to the next reader and wakes the scheduler
    if needed. This function is thread-safe. *)
val write : 'a t -> 'a -> [ `Closed | `Ok ]

(** [write_fill t fill] schedules [fill] after all values already written to
    [t] have been read. This function is thread-safe. *)
val write_fill : _ t -> Fiber.fill -> [ `Closed | `Ok ]

(** [read t] waits for the next value written to [t]. It returns [None] once
    [t] is closed and all previously written values have been read. *)
val read : 'a t -> 'a option Fiber.t

(** Close [t] and wake any readers that are waiting. This function is
    thread-safe and idempotent. *)
val close : _ t -> unit
