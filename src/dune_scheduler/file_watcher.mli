open Stdune

type t

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

(** The action that needs to be taken to shutdown the watcher. *)
val shutdown : t -> [ `Kill of Pid.t | `No_op | `Thunk of unit -> unit ]

val add_watch : t -> Path.t -> (unit, [ `Does_not_exist ]) result

module For_tests : sig
  val should_exclude : watch_exclusions:string list -> string -> bool
end
