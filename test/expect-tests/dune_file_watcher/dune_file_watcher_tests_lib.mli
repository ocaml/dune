val critical_section : Mutex.t -> f:(unit -> 'a) -> 'a
val init : unit -> unit

val print_events
  :  try_to_get_events:(unit -> Dune_file_watcher.Fs_memo_event.t list option)
  -> expected:int
  -> unit
