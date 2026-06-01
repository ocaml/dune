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
  type file_watcher =
    | Automatic
    | No_watcher

  val file_watcher_equal : file_watcher -> file_watcher -> bool

  exception Build_cancelled

  val go
    :  Config.t
    -> ?timeout:Time.Span.t
    -> ?file_watcher:file_watcher
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

    Fibers that are suspended on a call such as [wait_for_process] by the time
    [shutdown] is called will effectively never restart. If a fiber calls
    [wait_for_process] or any other function from this module that needs an
    external event to make progress, it will get suspended and will never
    restart. *)
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
val file_watcher : unit -> File_watcher.t option
val with_current_build_cancellation : Fiber.Cancel.t -> (unit -> 'a Fiber.t) -> 'a Fiber.t
