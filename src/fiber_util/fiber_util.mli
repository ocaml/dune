(** Utilities for working with the Fiber library. *)

open! Stdune

(** [Temp.Monad] instantiated to the Fiber monad. *)
module Temp : sig
  val with_temp_file :
       dir:Path.t
    -> prefix:string
    -> suffix:string
    -> f:(Path.t Or_exn.t -> 'a Fiber.t)
    -> 'a Fiber.t

  val with_temp_dir :
       parent_dir:Path.t
    -> prefix:string
    -> suffix:string
    -> f:(Path.t Or_exn.t -> 'a Fiber.t)
    -> 'a Fiber.t
end

(** Fiber cancellation *)
module Cancellation : sig
  (** This module provides a way to cancel long running computations.
      Cancellation is fully explicit and fibers must explicitely check for it at
      strategic points. *)

  type t

  val create : unit -> t

  (** Activate a cancellation.

      [fire] is idempotent, so calling [fire t] more than once has no effect. *)
  val fire : t -> unit Fiber.t

  (** Version of [fire] that is suitable to call from the [iter] callback of
      [Fiber.run]. *)
  val fire' : t -> Fiber.fill list

  (** Return whether the given cancellation has been fired. *)
  val fired : t -> bool

  type 'a outcome =
    | Cancelled of 'a
    | Not_cancelled

  (** [with_handler t ~on_cancellation f] runs [f ()] with a cancellation
      handler. If [t] is fired during the execution of [f], then
      [on_cancellation] is called.

      The aim of [on_cancellation] is to somehow cut short the execution of [f].
      A typical example is a function running an external command.
      [on_cancellation] might send a [KILL] signal to the command to abort its
      execution.

      If [f ()] finished before [t] is fired, then [on_cancellation] will never
      be invoked. *)
  val with_handler :
       t
    -> (unit -> 'a Fiber.t)
    -> on_cancellation:(unit -> 'b Fiber.t)
    -> ('a * 'b outcome) Fiber.t
end

(** A observer for a value that's changing over time *)
module Observer : sig
  (** An observer is something that let's one monitor a value that is changing
      over time.

      We call a value that is changing over time and can be observed an
      "observable". An "observer" is a routine watching this observable and
      getting notified when the value change. An observable may have several
      observers.

      The goal of an observer is to be up-to-date with the observable. It is not
      guaranteed that the observer will observe all possible intermediate values
      of the observable. For instance, if an observable changes several time in
      a row, by the time the observer is awoken it will receive the latest value
      of the observable. {!await} will only suspend the current fiber if the
      current value of the observable is the same as the last one that was
      reported.

      Given that an observer is stateful, it shouldn't be shared between
      different consumers. Each consumer that wants to watch an observable
      should create it's wn observer. Sharing an observer would lead to
      consumers not being up-to-date with the latest value of the observable. An
      observer should always be unsubscribed to via {!unsubscribe} when done
      with, otherwise the observable will continue updating it.

      Finally, sometimes the value is big but only changes a little at a time.
      For instance, an observable might represent "the set of current errors",
      and the producer will only add a remove one error at time. To avoid
      dumping a massive value on each update, we might instead want to send
      small "diffs". In such a case, values reported by {!await} would be diffs
      rather than full values and it is up to the consumer to reconstruct the
      full value if they want it. Successive diffs would still be squashed so
      that a call to {!await} returns them all at once. *)
  type 'a t

  (** [await t] returns the currently observed value. If the value hasn't been
      updated since the last [await t] call, this call will block. If the
      observable is closed, or unsubscribe is called this will return [None].
      After [None], all subsequent calls will return [None] immediately *)
  val await : 'a t -> 'a option Fiber.t

  (** [unsubscribe t] will make the current and all subsequent [await t] return
      [None] *)
  val unsubscribe : 'a t -> unit Fiber.t
end

(** A value that's changing over time and can be observed *)
module Observable : sig
  (** See {!Observer} for an overview of observables and observers. *)

  (** The reading end of an observable, to give to consumers. *)
  type 'a t

  (** The producer side of an observable. Only the produce should have a hold of
      it. *)
  type 'a sink

  (** [create ?combine initial] creates a new observable. [initial] is the
      initial value of the observable. [combine] is used to combine two
      successive values. For simple observables that receive the full value each
      time, this simply ignores the old value. This is the default for
      [combine], i.e. [(fun _ x -> x)].

      For diff-based observables used for larger values, this should squash the
      two diffs into a single one.

      Under the hood, values are not compared. Each time an observable is
      updated, this module assumes that the value is different. *)
  val create : ?combine:('a -> 'a -> 'a) -> 'a -> 'a t * 'a sink

  (** Update the value of an observable, or send a new diff for diff-based
      observable. *)
  val update : 'a sink -> 'a -> unit Fiber.t

  (** [close] an observable so that it can no longer receive updates. All
      current observers will be unsubscribed and pending {!await} calls will
      receive [None]. *)
  val close : 'a sink -> unit Fiber.t

  (** Create a new observer. The observer must be unsubscribed with
      [Observer.unsubscribe] when done with. *)
  val add_observer : 'a t -> 'a Observer.t
end
