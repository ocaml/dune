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
