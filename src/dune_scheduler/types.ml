open Stdune
module Task_id = Id.Make ()

module Async_io = struct
  type watcher =
    { io : Lev.Io.t
    ; mutable events : Lev.Io.Event.Set.t
    ; mutable read_ready : bool
    ; mutable write_ready : bool
    }

  type 'a or_cancel = ('a, [ `Cancelled | `Exn of exn ]) result

  type status =
    [ `Filled
    | `Waiting
    ]

  type t =
    { readers : (Fd.t, packed_task Queue.t) Table.t
    ; writers : (Fd.t, packed_task Queue.t) Table.t
    ; to_close : (Fd.t, unit Fiber.Ivar.t) Table.t
    ; mutex : Mutex.t
    ; scheduler_queue : Event.Queue.t
    ; loop : Lev.Loop.t
    ; wakeup : Lev.Async.t
    ; watchers : (Fd.t, watcher) Table.t
    ; mutable retired : watcher list
    ; timers : (Task_id.t, timer) Table.t
    ; mutable retired_timers : Lev.Timer.t list
    ; mutable thread : Thread.t option
    ; mutable started : bool
    ; mutable shutting_down : bool
    ; mutable destroyed : bool
    }

  and timer =
    { ivar : unit or_cancel Fiber.Ivar.t
    ; after : Time.Span.t
    ; deadline : Time.t
    ; select : t
    ; id : Task_id.t
    ; mutable watcher : Lev.Timer.t option
    ; mutable ready : bool
    ; mutable status : status
    }

  and ('a, 'label) task =
    { job : 'label -> Fd.t -> 'a
    ; ivar : 'a or_cancel Fiber.Ivar.t
    ; select : t
    ; what : [ `Read | `Write ]
    ; fds : Fd.t list
    ; id : Task_id.t
    ; mutable status : status
    }

  and packed_task = Task : (_, 'label) task * 'label -> packed_task
end

module Scheduler = struct
  type t =
    { job_throttle : Fiber.Throttle.t
    ; events : Event.Queue.t
    ; process_watcher : Process_watcher.t
    ; file_watcher : File_watcher.t option
    ; thread_pool : Thread_pool.t Lazy.t
    ; signal_watcher : Thread.t
    ; async_io : Async_io.t
    }

  let current : t option ref = ref None
  let t_opt () = !current
  let t () = Option.value_exn !current
end
