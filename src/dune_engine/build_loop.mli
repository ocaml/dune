open Import

(** State shared by watch-mode build loops. *)
type t

(** A build request run by the watch-mode build loop. *)
type step =
  run_id:Run_id.t
  -> restart_started_at:Time.t option
  -> (unit, [ `Already_reported ]) Result.t Fiber.t

(** [run f] initializes watch-mode state and runs [f] with it. *)
val run : (t -> 'a Fiber.t) -> 'a Fiber.t

(** [poll t step] runs [step] in a loop.

    If any source files change in the middle of iteration, it gets canceled.

    If [Scheduler.shutdown] is called, the current build will be canceled and
    new builds will not start. *)
val poll : t -> step -> unit Fiber.t

(** [poll_passive] is similar to [poll], but it can be used to drive the
    polling loop explicitly instead of starting new iterations automatically.

    The fiber [get_build_request] is run at the beginning of every iteration to
    wait for the build signal. *)
val poll_passive
  :  t
  -> get_build_request:(step * Build_outcome.t Fiber.Ivar.t) Fiber.t
  -> unit Fiber.t

module For_tests : sig
  val wait_for_build_input_change : t -> unit Fiber.t
  val inject_memo_invalidation : t -> Memo.Invalidation.t -> unit Fiber.t
end
