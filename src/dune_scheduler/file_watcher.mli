open Stdune

type t

(** Standard path exclusions used by the file watcher. *)
val standard_watch_exclusions : string list

(** Create a new file watcher. *)
val create
  :  ?fsevents_debounce:Time.Span.t
  -> watch_exclusions:string list
  -> event_queue:Event.Queue.t
  -> unit
  -> t

val close : t -> unit
val read : t -> Event.File_watcher_event.t list option Fiber.t
val flush : t -> unit Fiber.t

(** Child processes owned by the file watcher. *)
val child_pids : t -> Pid.t option

val shutdown : t -> unit
val add_watch : t -> Path.t -> (unit, [ `Does_not_exist ]) result
