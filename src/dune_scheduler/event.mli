open Stdune

type job =
  { pid : Pid.t
  ; ivar : Proc.Process_info.t Fiber.Ivar.t
  }

val dyn_of_job : job -> Dyn.t

type build_input_change =
  | Fs_event of Dune_file_watcher.Fs_memo_event.t
  | Invalidation of Memo.Invalidation.t

type t =
  | File_watcher_task of (unit -> Dune_file_watcher.Event.t list)
  | Build_inputs_changed of build_input_change Nonempty_list.t
  | File_system_sync of Dune_file_watcher.Sync_id.t
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

  (** Number of jobs for which the status hasn't been reported yet .*)
  val pending_jobs : t -> int

  val send_worker_task_completed : t -> Fiber.fill -> unit
  val send_worker_tasks_completed : t -> Fiber.fill list -> unit
  val register_worker_task_started : t -> unit
  val cancel_work_task_started : t -> unit
  val send_file_watcher_task : t -> (unit -> Dune_file_watcher.Event.t list) -> unit

  (** It's a bit weird to have both this and [send_file_watcher_task]. The
      reason is that [send_file_watcher_task] uses [send_file_watcher_events]
      internally. *)
  val send_file_watcher_events : t -> Dune_file_watcher.Event.t list -> unit

  val send_invalidation_event : t -> Memo.Invalidation.t -> unit
  val send_job_completed : t -> job -> Proc.Process_info.t -> unit
  val send_job_completed_ready : t -> unit
  val send_shutdown : t -> Shutdown.Reason.t -> unit
  val send_timers_completed : t -> Fiber.fill Nonempty_list.t -> unit
  val yield_if_there_are_pending_events : t -> unit Fiber.t
end
