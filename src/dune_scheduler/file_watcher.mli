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

module Sync : sig
  type t

  val await : t -> unit Fiber.t
end

val start_sync : t -> Sync.t

(** Retry the sync if its current attempt has expired. [`Wait_for delay]
    requests another check after [delay]. [`Finished] means no further retries
    are needed; [Sync.await] provides the sync's success or failure. *)
val retry_sync : t -> Sync.t -> [ `Finished | `Wait_for of Time.Span.t ] Fiber.t

(** Child processes owned by the file watcher. *)
val child_pids : t -> Pid.t option

val shutdown : t -> unit
val add_watch : t -> Path.t -> (unit, [ `Does_not_exist ]) result
