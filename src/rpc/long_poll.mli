open Stdune

(** Raised internally when an in-flight long-poll request is cancelled by the client. *)
exception Poll_cancelled

module Poller : sig
  type t

  val create : unit -> t
  val cancelled : t -> bool
  val fire_cancel : t -> unit Fiber.t
end

(** A source of state observed by long polling. [Svar] uses [Fiber.Svar.wait] to block
    until the value changes; [Computed] reads the value from a thunk and polls it every
    [poll_every] (used for state held in a plain [ref], which cannot wake waiters on its
    own). *)
module Source : sig
  type 'a t =
    | Svar of 'a Fiber.Svar.t
    | Computed of
        { get : unit -> 'a
        ; poll_every : Time.Span.t
        }

  val read : 'a t -> 'a
  val wait : 'a t -> until:('a -> bool) -> unit Fiber.t
end

module Status : sig
  type 'a t =
    | Idle of 'a
    | Waiting
end

module Map : Map.S with type key = Poller.t

(** [make_on_poll map source ~equal ~diff session poller] computes the response for one
    poll request: it waits (respecting [poller]'s cancellation) until [source]'s value
    satisfies [equal]-difference from the last one served, then records the new value in
    [map] and returns [diff ~last ~now]. *)
val make_on_poll
  :  'a Status.t Map.t ref
  -> 'a Source.t
  -> equal:('a -> 'a -> bool)
  -> diff:(last:'a option -> now:'a -> 'b)
  -> 'session
  -> Poller.t
  -> 'b option Fiber.t
