val init : unit -> unit

val print_events
  :  try_to_get_events:(unit -> Dune_scheduler.File_watcher.Fs_memo_event.t list option)
  -> expected:int
  -> unit
