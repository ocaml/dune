(** A priority queue with mutable, shared priority handles.

    Each waiting value is ordered independently. Updating a shared priority
    handle immediately reprioritizes all values associated with it without
    changing their enqueue metadata. *)

module Priority : sig
  type t =
    { primary : int
    ; secondary : int
    ; tertiary : int
    }

  val make : primary:int -> secondary:int -> tertiary:int -> t
  val zero : t
  val of_int : int -> t
  val compare : t -> t -> Ordering.t
  val equal : t -> t -> bool
  val to_dyn : t -> Dyn.t
end

module Enqueue : sig
  type t =
    { sequence : int
    ; random_key : int
    ; attempt_id : int
    }
end

module type S = sig
  type 'a t
  type 'a priority

  val create : unit -> 'a t

  (** Create a queue where [order_key] controls the order among values with
      equal semantic priorities. Higher keys are returned first. *)
  val create_with_order_key : order_key:(Enqueue.t -> int) -> 'a t

  (** Create a priority handle owned by the queue. *)
  val create_priority : ?priority:int -> 'a t -> 'a priority

  val create_rank : rank:Priority.t -> 'a t -> 'a priority

  (** Return the current value of the priority handle. *)
  val priority : 'a priority -> int

  val rank : 'a priority -> Priority.t

  (** Set the handle's priority. If the handle has queued values, they are
      reprioritized immediately without changing their enqueue metadata. *)
  val set_priority : 'a priority -> int -> unit

  val set_rank : 'a priority -> Priority.t -> unit

  (** Increase the handle's primary priority by one. If the handle has queued
      values, they are reprioritized immediately. The priority saturates at
      [max_int]. *)
  val increase_priority : 'a priority -> unit

  (** [increase_priority_by t n] increases the primary priority by [n]. [n]
      must be non-negative. Reprioritization is eager and the value saturates
      at [max_int]. *)
  val increase_priority_by : 'a priority -> int -> unit

  (** Raise a code error if the priority handle is owned by another queue. *)
  val check_priority : 'a t -> 'a priority -> unit

  (** Add a value associated with the priority handle. The handle must have
      been created by this queue. *)
  val push : 'a t -> 'a priority -> 'a -> unit

  (** Return the highest-ranked value. *)
  val pop : 'a t -> 'a option

  (** Return the highest-ranked value without removing it. *)
  val peek : 'a t -> 'a option

  (** Return the semantic priority of the next value. *)
  val max_priority : 'a t -> int option

  val max_rank : 'a t -> Priority.t option
  val is_empty : 'a t -> bool
  val length : 'a t -> int
end

include S
