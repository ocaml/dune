open Stdune

type job =
  { pid : Pid.t
  ; is_process_group_leader : bool
  ; ivar : Proc.Process_info.t Fiber.Ivar.t
  }

val dyn_of_job : job -> Dyn.t

module Fs_memo_event : sig
  type kind =
    | Created
    | Deleted
    | File_changed
    | Unknown

  type t = private
    { path : Path.t
    ; kind : kind
    }

  val create : kind:kind -> path:Path.t -> t
  val to_dyn : t -> Dyn.t
end

module Sync_id : sig
  type t

  val equal : t -> t -> bool
  val hash : t -> int
  val gen : unit -> t
  val to_int : t -> int
  val to_dyn : t -> Dyn.t
end

module File_watcher_event : sig
  type t =
    | Fs_memo_event of Fs_memo_event.t
    | Queue_overflow
    | Sync of Sync_id.t
    | Watcher_terminated
end

type build_input_change =
  | Fs_event of Fs_memo_event.t
  | Invalidation of Memo.Invalidation.t

type t =
  | Build_inputs_changed of build_input_change Nonempty_list.t
  | File_system_sync of Sync_id.t
  | File_system_watcher_terminated
  | Shutdown of Shutdown.Reason.t
  | Fiber_fill_ivar of Fiber.fill
  | Job_complete_ready

module Queue : sig
  type event := t
  type t

  val to_dyn : t -> Dyn.t
  val create : unit -> t

  (** Return the next event. File changes event are always flattened and
      returned first. *)
  val next : t -> event

  (** Pending worker tasks *)
  val pending_worker_tasks : t -> int

  (** Register the fact that a job was started. *)
  val register_job_started : t -> unit

  (** Register the fact that a job was finished. *)
  val finish_job : t -> unit

  (** Number of jobs for which the status hasn't been reported yet. *)
  val pending_jobs : t -> int

  val send_worker_task_completed : t -> Fiber.fill -> unit
  val send_worker_tasks_completed : t -> Fiber.fill list -> unit
  val register_worker_task_started : t -> unit
  val cancel_work_task_started : t -> unit
  val send_file_watcher_events : t -> File_watcher_event.t list -> unit
  val send_invalidation_event : t -> Memo.Invalidation.t -> unit
  val send_job_completed : t -> job -> Proc.Process_info.t -> unit
  val send_job_completed_ready : t -> unit
  val send_shutdown : t -> Shutdown.Reason.t -> unit
  val yield_if_there_are_pending_events : t -> unit Fiber.t
end
