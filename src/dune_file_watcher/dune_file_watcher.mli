open! Stdune
module Inotify_lib := Async_inotify_for_dune.Async_inotify

type t

module Event : sig
  type t =
    | File_changed of Path.t
    | Inotify_event of Inotify_lib.Event.t
    | Sync
    | Watcher_terminated
end

val inotify_event_paths : Inotify_lib.Event.t -> string list

module Scheduler : sig
  (** Hook into the fiber scheduler. *)
  type t =
    { spawn_thread : (unit -> unit) -> unit
          (** We spawn threads through this function in case the scheduler wants
              to block signals *)
    ; thread_safe_send_events : Event.t list -> unit
          (** Send a list of events to the scheduler from a separate system
              thread. *)
    ; thread_safe_send_job : (unit -> unit) -> unit
          (** Send a thunk to be executed on the scheduler thread. *)
    }
end

(** Create a new file watcher. [debounce_interval] is measured in seconds and it
    controls the minimum time between calls to
    [scheduler.thread_safe_send_files_changed]. *)
val create_external :
  root:Path.t -> debounce_interval:float option -> scheduler:Scheduler.t -> t

(** Create a new file watcher with default settings. *)
val create_default : scheduler:Scheduler.t -> t

(** The action that needs to be taken to shutdown the watcher. *)
val shutdown : t -> [ `Kill of Pid.t | `No_op ]

val wait_for_initial_watches_established_blocking : t -> unit

(** Cause a [Sync] event to be propagated through the notification sybsystem to
    attemt to make sure that we've processed all the events that happened so
    far. *)
val emit_sync : unit -> unit

module For_tests : sig
  val suspend : t -> unit

  val resume : t -> unit
end

val add_watch : t -> Path.t -> unit
