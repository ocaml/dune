(** Scheduling *)

open! Import

module Config : sig
  module Display : sig
    type verbosity =
      | Quiet  (** Only display errors *)
      | Short  (** One line per command *)
      | Verbose  (** Display all commands fully *)

    type t =
      { status_line : bool
      ; verbosity : verbosity
      }

    val all : (string * t) list

    val to_dyn : t -> Dyn.t

    (** The console backend corresponding to the selected display mode *)
    val console_backend : t -> Console.Backend.t
  end

  type t =
    { concurrency : int
    ; display : Display.t
    ; stats : Dune_stats.t option
    ; insignificant_changes : [ `Ignore | `React ]
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
    module Signal : sig
      (* TODO move this stuff into stdune? *)
      type t =
        | Int
        | Quit
        | Term
    end

    module Reason : sig
      type t =
        | Requested
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
  val poll_passive :
       get_build_request:(step * Build_outcome.t Fiber.Ivar.t) Fiber.t
    -> unit Fiber.t

  val go :
       Config.t
    -> ?timeout:float
    -> ?file_watcher:file_watcher
    -> on_event:(Config.t -> Event.t -> unit)
    -> (unit -> 'a Fiber.t)
    -> 'a
end

module Worker : sig
  (** A worker is a thread that runs submitted tasks *)
  type t

  val create : unit -> t Fiber.t

  val task :
       t
    -> f:(unit -> 'a)
    -> ('a, [ `Exn of Exn_with_backtrace.t | `Stopped ]) result Fiber.t

  (** Should be used for tasks never raise and always complete before stop is
      called *)
  val task_exn : t -> f:(unit -> 'a) -> 'a Fiber.t

  val stop : t -> unit
end

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
val wait_for_process :
     ?timeout:float
  -> ?is_process_group_leader:bool
  -> Pid.t
  -> Proc.Process_info.t Fiber.t

val yield_if_there_are_pending_events : unit -> unit Fiber.t

(** If the current build was cancelled, raise
    [Memo.Non_reproducible Run.Build_cancelled]. *)
val abort_if_build_was_cancelled : unit Fiber.t

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

val inject_memo_invalidation : Memo.Invalidation.t -> unit Fiber.t

(** [sleep duration] wait for [duration] to elapse. Sleepers are checked for
    wake up at a rate of once per 0.1 seconds. So [duration] should be at least
    this long. *)
val sleep : float -> unit Fiber.t

(** Wait until all file system changes that happened so far have been
    acknowledged by the scheduler. *)
val flush_file_watcher : unit -> unit Fiber.t

(** Wait for a build input to change. If a build input change was seen but
    hasn't been handled yet, return immediately.

    Return even for build input change that are not significant, so that RPC
    clients may observe that Dune reacted to a file change. This is needed for
    benchmarking the watch mode of Dune. *)
val wait_for_build_input_change : unit -> unit Fiber.t
