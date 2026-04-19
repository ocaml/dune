open Stdune
module Task_id = Id.Make ()

module Async_io = struct
  type watcher =
    { io : Lev.Io.t
    ; mutable events : Lev.Io.Event.Set.t
    ; mutable read_ready : bool
    ; mutable write_ready : bool
    }

  type t =
    { readers : (Fd.t, packed_task Queue.t) Table.t
    ; writers : (Fd.t, packed_task Queue.t) Table.t
    ; to_close : (Fd.t, Fiber.fill list) Table.t
    ; mutex : Mutex.t
    ; scheduler_queue : Event.Queue.t
    ; loop : Lev.Loop.t
    ; wakeup : Lev.Async.t
    ; watchers : (Fd.t, watcher) Table.t
    ; mutable retired : watcher list
    ; mutable thread : Thread.t option
    ; mutable started : bool
    ; mutable shutting_down : bool
    ; mutable destroyed : bool
    }

  and ('a, 'label) task =
    { job : 'label -> Fd.t -> 'a
    ; ivar : ('a, [ `Cancelled | `Exn of exn ]) result Fiber.Ivar.t
    ; select : t
    ; what : [ `Read | `Write ]
    ; fds : Fd.t list
    ; id : Task_id.t
    ; mutable status : [ `Filled | `Waiting ]
    }

  and packed_task = Task : (_, 'label) task * 'label -> packed_task
end

module Scheduler = struct
  module Build_outcome = struct
    type t =
      | Success
      | Failure
  end

  module Handler = struct
    module Event = struct
      type t =
        | Tick
        | Source_files_changed of { details_hum : string list }
        | Build_interrupted
        | Build_finish of Build_outcome.t
    end

    type t = Event.t -> unit
  end

  type status =
    | (* We are not doing a build. Just accumulating invalidations until the next
       build starts. *)
      Standing_by
    | (* Running a build *)
      Building of Fiber.Cancel.t
    | (* Cancellation requested. Build jobs are immediately rejected in this
       state *)
      Restarting_build

  type t =
    { alarm_clock : Alarm_clock.t Lazy.t
    ; mutable status : status
    ; mutable invalidation : Memo.Invalidation.t
    ; mutable run_id_state : Run_id.State.t
    ; mutable watch_restart_started_at : Time.t option
    ; handler : Handler.t
    ; job_throttle : Fiber.Throttle.t
    ; events : Event.Queue.t
    ; process_watcher : Process_watcher.t
    ; file_watcher : File_watcher.t option
    ; fs_syncs : (File_watcher.Sync_id.t, unit Fiber.Ivar.t) Table.t
    ; mutable build_inputs_changed : Trigger.t
    ; mutable cancel : Fiber.Cancel.t
    ; thread_pool : Thread_pool.t Lazy.t
    ; signal_watcher : Thread.t
    ; async_io : Async_io.t
    }

  let current : t option ref = ref None
  let t_opt () = !current
  let t () = Option.value_exn !current
end
