open Stdune

type job =
  { pid : Pid.t
  ; is_process_group_leader : bool
  ; ivar : Proc.Process_info.t Fiber.Ivar.t
  }

val dyn_of_job : job -> Dyn.t

module Fs_memo_event : sig
  type t = private
    { path : Path.t
    ; kind : Dune_trace.File_watcher_event.kind
    }

  val create : kind:Dune_trace.File_watcher_event.kind -> path:Path.t -> t
  val to_dyn : t -> Dyn.t
end

module File_watcher_event : sig
  type t =
    | Fs_memo_event of Fs_memo_event.t
    | Queue_overflow
end

type t =
  | Shutdown of Shutdown.Reason.t
  | Fiber_fill_ivar of Fiber.fill
  | Job_complete_ready

module Queue : sig
  type event := t
  type t

  val to_dyn : t -> Dyn.t
  val create : unit -> t

  (** Return the next event. *)
  val next : t -> event

  val send_worker_tasks_completed : t -> Fiber.fill list -> unit
  val send_job_completed : t -> job -> Proc.Process_info.t -> unit
  val send_job_completed_ready : t -> unit
  val send_shutdown : t -> Shutdown.Reason.t -> unit
  val yield_if_there_are_pending_events : t -> unit Fiber.t
end
