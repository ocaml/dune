(** Scheduling *)

open! Import
open Stdune

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
    ; rpc : Dune_rpc.Where.t option
    ; stats : Dune_stats.t option
    }

  (** [add_to_env env] adds to [env] the environment variable that describes
      where the current RPC server is listening (if it's running) *)
  val add_to_env : t -> Env.t -> Env.t
end

module Run : sig
  module Event : sig
    type build_result =
      | Success
      | Failure

    type t =
      | Tick
      | Source_files_changed
      | Build_interrupted
      | Build_finish of build_result
  end

  type file_watcher =
    | Detect_external
    | No_watcher

  (** Raised when [go] terminates due to the user requesting a shutdown via rpc.
      The caller needs to know about this to set the exit code to 0 for such
      cases *)
  exception Shutdown_requested

  (** Runs [once] in a loop, executing [finally] after every iteration, even if
      Fiber.Never was encountered.

      If any source files change in the middle of iteration, it gets canceled.

      If [shutdown] is called, the current build will be canceled and new builds
      will not start. *)
  val poll : (unit -> [ `Continue | `Stop ] Fiber.t) -> unit Fiber.t

  val go :
       Config.t
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

  val stop : t -> unit
end

type t

(** Get the instance of the scheduler that runs the current fiber. *)
val t : unit -> t Fiber.t

(** [with_job_slot f] waits for one job slot (as per [-j <jobs] to become
    available and then calls [f]. *)
val with_job_slot : (Config.t -> 'a Fiber.t) -> 'a Fiber.t

(** Wait for the following process to terminate *)
val wait_for_process : Pid.t -> Proc.Process_info.t Fiber.t

(** Make the scheduler ignore next change to a certain file in watch mode.

    This is used with promoted files that are copied back to the source tree
    after generation *)
val ignore_for_watch : Path.t -> unit Fiber.t

(** Number of jobs currently running in the background *)
val running_jobs_count : t -> int

(** Start the shutdown sequence. Among other things, it causes Dune to cancel
    the current build and stop accepting RPC clients. *)
val shutdown : unit -> unit Fiber.t
