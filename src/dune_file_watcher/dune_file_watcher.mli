open! Stdune

type t

module Event : sig
  type t =
    | File_changed of Path.t
    | Watcher_terminated
end

module Scheduler : sig
  (** Hook into the fiber scheduler. *)
  type t =
    { thread_safe_send_events : Event.t list -> unit
          (** Send a list of events to the scheduler from a separate system
              thread.. *)
    ; spawn_thread : (unit -> unit) -> unit
          (** We spawn threads through this function in case the scheduler wants
              to block signals *)
    }
end

(** Create a new file watcher. [debounce_interval] is measured in seconds and it
    controls the minimum time between calls to
    [scheduler.thread_safe_send_files_changed]. *)
val create :
  root:Path.t -> debounce_interval:float option -> scheduler:Scheduler.t -> t

(** Create a new file watcher with default settings. *)
val create_default : scheduler:Scheduler.t -> t

(** Pid of the external file watcher process *)
val pid : t -> Pid.t

val wait_watches_established_blocking : t -> unit

module For_tests : sig
  val suspend : t -> unit

  val resume : t -> unit
end
