open Import

(** A build request run by the watch-mode build loop. *)
type step = (unit, [ `Already_reported ]) Result.t Fiber.t

(** [poll step] runs [step] in a loop.

    If any source files change in the middle of iteration, it gets canceled.

    If [Scheduler.shutdown] is called, the current build will be canceled and
    new builds will not start. *)
val poll : step -> unit Fiber.t

(** [poll_passive] is similar to [poll], but it can be used to drive the
    polling loop explicitly instead of starting new iterations automatically.

    The fiber [get_build_request] is run at the beginning of every iteration to
    wait for the build signal. *)
val poll_passive
  :  get_build_request:(step * Build_outcome.t Fiber.Ivar.t) Fiber.t
  -> unit Fiber.t
