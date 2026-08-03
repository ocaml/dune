open Stdune

val init : unit -> unit

val create_watcher
  :  ?fsevents_debounce:Time.Span.t
  -> watch_exclusions:string list
  -> unit
  -> Dune_scheduler.File_watcher.t
     * (unit -> Dune_scheduler.Event.Fs_memo_event.t list option)

val print_events
  :  try_to_get_events:(unit -> Dune_scheduler.Event.Fs_memo_event.t list option)
  -> expected:int
  -> unit
