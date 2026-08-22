(** A priority queue with mutable, shared priority handles.

    Values associated with the same priority handle are returned in FIFO order.
    Across handles with the same priority, values are also returned in FIFO
    order. *)

module type S = sig
  type 'a t
  type 'a priority

  val create : unit -> 'a t

  (** Create a priority handle owned by the queue. *)
  val create_priority : ?priority:int -> 'a t -> 'a priority

  (** Return the current value of the priority handle. *)
  val priority : 'a priority -> int

  (** Set the handle's priority. If the handle has queued values, they are
      reprioritized immediately without changing their FIFO age. *)
  val set_priority : 'a priority -> int -> unit

  (** Increase the handle's priority by one. If the handle has queued values,
      they are reprioritized immediately. The priority saturates at [max_int]. *)
  val increase_priority : 'a priority -> unit

  (** [increase_priority_by t n] increases the priority by [n]. [n] must be
      non-negative. Reprioritization is eager and the value saturates at
      [max_int]. *)
  val increase_priority_by : 'a priority -> int -> unit

  (** Raise a code error if the priority handle is owned by another queue. *)
  val check_priority : 'a t -> 'a priority -> unit

  (** Add a value associated with the priority handle. The handle must have
      been created by this queue. *)
  val push : 'a t -> 'a priority -> 'a -> unit

  (** Return the highest-priority value. *)
  val pop : 'a t -> 'a option

  (** Return the highest-priority value without removing it. *)
  val peek : 'a t -> 'a option

  (** Return the priority of the next value. *)
  val max_priority : 'a t -> int option

  val is_empty : 'a t -> bool
  val length : 'a t -> int
end

include S
