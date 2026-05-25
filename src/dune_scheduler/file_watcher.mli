open Stdune

type t

(** Create a new file watcher with default settings. *)
val create_default
  :  ?fsevents_debounce:Time.Span.t
  -> watch_exclusions:string list
  -> event_queue:Event.Queue.t
  -> unit
  -> t

(** The action that needs to be taken to shutdown the watcher. *)
val shutdown : t -> [ `Kill of Pid.t | `No_op | `Thunk of unit -> unit ]

(** Cause a sync event to be propagated through the notification subsystem to
    attempt to make sure that we've processed all the events that happened so
    far. *)
val emit_sync : t -> Event.Sync_id.t

val add_watch : t -> Path.t -> (unit, [ `Does_not_exist ]) result

module For_tests : sig
  val should_exclude : watch_exclusions:string list -> string -> bool
end
