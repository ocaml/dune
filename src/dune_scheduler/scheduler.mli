(** Scheduling *)

open Import

module Config : sig
  type t =
    { concurrency : int
    ; print_ctrl_c_warning : bool
    ; watch_exclusions : string list
    }
end

module Run : sig
  module Event : sig
    type t = Build_interrupted
  end

  type file_watcher =
    | Automatic
    | No_watcher

  val file_watcher_equal : file_watcher -> file_watcher -> bool

  exception Build_cancelled

  val go
    :  Config.t
    -> ?timeout:Time.Span.t
    -> ?file_watcher:file_watcher
    -> on_event:(Event.t -> unit)
    -> (unit -> 'a Fiber.t)
    -> 'a
end

(** [async f] runs [f] inside a background thread pool *)
val async : (unit -> 'a) -> ('a, Exn_with_backtrace.t) result Fiber.t

(** [async_exn f] runs [f] inside a background thread pool *)
val async_exn : (unit -> 'a) -> 'a Fiber.t

type t

(** Get the instance of the scheduler that runs the current fiber. *)
val t : unit -> t

(** [with_job_slot f] waits for one job slot (as per [-j <jobs] to become
    available and then calls [f]. *)
val with_job_slot : (unit -> 'a Fiber.t) -> 'a Fiber.t

(** Wait for the following process to terminate. If [is_process_group_leader] is
    true, kill the entire process group instead of just the process in case of
    timeout. *)
val wait_for_process
  :  ?timeout:Time.Span.t
  -> is_process_group_leader:bool
  -> Pid.t
  -> Proc.Process_info.t Fiber.t

type termination_reason =
  | Normal
  | Cancel
  | Timeout

val wait_for_build_process
  :  ?timeout:Time.Span.t
  -> is_process_group_leader:bool
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
val shutdown : unit -> unit

(** Cancel the current build. Superficially, this function is like [shutdown]
    in that it stops the build early, but it is different because the [Run.go]
    call is allowed to complete its fiber. In this respect, the behavior is
    similar to what happens on file system events in polling mode. *)
val cancel_current_build : unit -> unit Fiber.t

(** [sleep duration] waits for [duration] to elapse. *)
val sleep : Time.Span.t -> unit Fiber.t

val spawn_thread : name:string -> (unit -> unit) -> Thread.t
val flush_file_watcher : unit -> unit Fiber.t

module Build_loop : sig
  type t

  type build_finish =
    | Finished of { restart_duration : Time.Span.t option }
    | Restarting

  val is_watch_mode : unit -> bool
  val start_build : unit -> Run_id.t * [ `Restart of bool ]
  val finish_build : stop:Time.t -> build_finish
  val init : unit -> t Fiber.t
  val pending_invalidation : t -> Memo.Invalidation.t
  val start_iteration : t -> unit
  val finish_iteration : t -> [ `Done | `Restart ]
  val wait_for_build_input_change : t -> unit Fiber.t
end

(** [set_fs_memo_impl] registers the file system memoization callbacks.
    This must be called by dune_engine at initialization before starting
    the scheduler to enable proper file system event handling. *)
val set_fs_memo_impl
  :  handle_fs_event:(File_watcher.Fs_memo_event.t -> Memo.Invalidation.t)
  -> init:(dune_file_watcher:File_watcher.t option -> Memo.Invalidation.t)
  -> unit

module For_tests : sig
  (** Wait for a build input to change. If a build input change was seen but
      hasn't been handled yet, return immediately.

      Return even for build input change that are not significant, so that RPC
      clients may observe that Dune reacted to a file change. This is needed
      for benchmarking the watch mode of Dune. *)
  val wait_for_build_input_change : unit -> unit Fiber.t

  val inject_memo_invalidation : Memo.Invalidation.t -> unit Fiber.t
end
