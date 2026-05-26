val init : unit -> unit

val create_event_queue
  :  unit
  -> Dune_scheduler.Event.Queue.t
     * (unit -> Dune_scheduler.Event.Fs_memo_event.t list option)

val print_events
  :  try_to_get_events:(unit -> Dune_scheduler.Event.Fs_memo_event.t list option)
  -> expected:int
  -> unit
