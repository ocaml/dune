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

(** The set of pollers belonging to a single long-poll implementation. Each poller
    progresses [Initialized] -> [Active] and may become [Cancelled]. *)
module Active_set : sig
  type 'a t

  val create : unit -> 'a t

  (** [cancel t poller] marks [poller] as cancelled, running its [on_cancel] hook with the
      source's current value. *)
  val cancel : 'a t -> Poller.t -> unit Fiber.t
end

(** [make_on_poll set source ~equal ~diff session poller] computes the response for one
    poll request against [set]: it waits (respecting [poller]'s cancellation) until
    [source]'s value differs (per [equal]) from the last one served, records it, and
    returns [diff ~last ~now]. *)
val make_on_poll
  :  'a Active_set.t
  -> 'a Source.t
  -> equal:('a -> 'a -> bool)
  -> diff:(last:'a option -> now:'a -> 'b)
  -> 'session
  -> Poller.t
  -> 'b option Fiber.t
