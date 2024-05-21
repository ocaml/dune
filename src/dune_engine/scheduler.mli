(** Scheduling *)

open! Import

module Config : sig
  type t =
    { concurrency : int
    ; stats : Dune_stats.t option
    ; print_ctrl_c_warning : bool
    ; watch_exclusions : string list
    }
end

module Run : sig
  module Build_outcome : sig
    type t =
      | Success
      | Failure
  end

  module Event : sig
    type t =
      | Tick
      | Source_files_changed of { details_hum : string list }
      | Build_interrupted
      | Build_finish of Build_outcome.t
  end

  type file_watcher =
    | Automatic
    | No_watcher

  module Shutdown : sig
    module Reason : sig
      type t =
        | Requested
        | Timeout
        | Signal of Signal.t
    end

    (** Raised when [go] terminates due to the user requesting a shutdown via
        rpc or raising a signal. The caller needs to know about this to set the
        exit code correctly *)
    exception E of Reason.t
  end

  exception Build_cancelled

  type step = (unit, [ `Already_reported ]) Result.t Fiber.t

  (** [poll once] runs [once] in a loop.

      If any source files change in the middle of iteration, it gets canceled.

      If [shutdown] is called, the current build will be canceled and new builds
      will not start. *)
  val poll : step -> unit Fiber.t

  (** [poll_passive] is similar to [poll], but it can be used to drive the
      polling loop explicitly instead of starting new iterations automatically.

      The fiber [get_build_request] is run at the beginning of every iteration
      to wait for the build signal. *)
  val poll_passive
    :  get_build_request:(step * Build_outcome.t Fiber.Ivar.t) Fiber.t
    -> unit Fiber.t

  val go
    :  Config.t
    -> ?timeout:float
    -> ?file_watcher:file_watcher
    -> on_event:(Config.t -> Event.t -> unit)
    -> (unit -> 'a Fiber.t)
    -> 'a
end

(** [async f] runs [f] inside a background thread pool *)
val async : (unit -> 'a) -> ('a, Exn_with_backtrace.t) result Fiber.t

(** [async_exn f] runs [f] inside a background thread pool *)
val async_exn : (unit -> 'a) -> 'a Fiber.t

type t

(** Get the instance of the scheduler that runs the current fiber. *)
val t : unit -> t Fiber.t

(** [with_job_slot f] waits for one job slot (as per [-j <jobs] to become
    available and then calls [f]. The cancellation token is provided to [f] to
    avoid doing some work if the job's result is no longer necessary. *)
val with_job_slot : (Fiber.Cancel.t -> Config.t -> 'a Fiber.t) -> 'a Fiber.t

(** Wait for the following process to terminate. If [is_process_group_leader] is
    true, kill the entire process group instead of just the process in case of
    timeout. *)
val wait_for_process
  :  ?timeout:float
  -> ?is_process_group_leader:bool
  -> Pid.t
  -> Proc.Process_info.t Fiber.t

type termination_reason =
  | Normal
  | Cancel

val wait_for_build_process
  :  ?timeout:float
  -> ?is_process_group_leader:bool
  -> Pid.t
  -> (Proc.Process_info.t * termination_reason) Fiber.t

(** Number of jobs currently running in the background *)
val running_jobs_count : t -> int

(** Abort any on-going [Run.go], making it raise [Run.Shutdown_requested].

    To understand the exact effect of [shutdown], one needs to understand how
    fibers make progress. Fibers typically make progress by "waves". Each fiber
    runs for as long as it can until it can no longer make progress on its own.
    Typically, because it needs to wait for an external event.

    When no more fiber can progress, the scheduler waits for an external event.
    When it receives one event, it wakes up the corresponding [Fiber.Ivar.t]
    which triggers another wave of progress happens.

    When [shutdown] is called, the current wave of progress will continue its
    course until no more fiber can progress on its own. Then the scheduler will
    do a bit cleanup, such as killing with [SIGKILL] all running external
    processes and will then raise [Run.Shutdown_requested].

    Fibers that are suspended on a call such as [wait_for_process] or
    [wait_for_build_input_change] by the time [shutdown] is called will
    effectively never restart. If a fiber calls [wait_for_process] or any other
    function from this module that needs an external event to make progress, it
    will get suspended and will never restart. *)
val shutdown : unit -> unit Fiber.t

(** Cancel the current build. Superficially, this function is like [shutdown]
    in that it stops the build early, but it is different because the [Run.go]
    call is allowed to complete its fiber. In this respect, the behavior is
    similar to what happens on file system events in polling mode. *)
val cancel_current_build : unit -> unit Fiber.t

val inject_memo_invalidation : Memo.Invalidation.t -> unit Fiber.t

(** [sleep duration] wait for [duration] to elapse. Sleepers are checked for
    wake up at a rate of once per 0.1 seconds. So [duration] should be at least
    this long. *)
val sleep : float -> unit Fiber.t

val stats : unit -> Dune_stats.t option Fiber.t

(** Wait for a build input to change. If a build input change was seen but
    hasn't been handled yet, return immediately.

    Return even for build input change that are not significant, so that RPC
    clients may observe that Dune reacted to a file change. This is needed for
    benchmarking the watch mode of Dune. *)
val wait_for_build_input_change : unit -> unit Fiber.t

val spawn_thread : (unit -> unit) -> unit
