(** Minimal bindings for fsevents on macos.

    We only bind to the subset of fsevents relevant to dune. *)

val available : unit -> bool

module RunLoop : sig
  type t

  val in_current_thread : unit -> t

  val run_current_thread : t -> (unit, exn) result
end

module Event : sig
  module Id : sig
    (** monotonically increasing id *)
    type t
  end

  (** file system event *)
  type t

  val to_dyn_raw : t -> Dyn.t

  val to_dyn : t -> Dyn.t

  (** [id t] return the id of the event *)
  val id : t -> Id.t

  (** [path t] returns the file path this event applies to *)
  val path : t -> string

  type kind =
    | Dir  (** directory *)
    | File  (** file event *)
    | Dir_and_descendants
        (** non-specific directory event. all descendants of this directory are
            invalidated *)

  val dyn_of_kind : kind -> Dyn.t

  (** [kind t] describes the [kind] of [path t] *)
  val kind : t -> kind

  type action =
    | Create (* [path t] guaranteed to exist *)
    | Remove (* [path t] guaranteed to be absent *)
    | Modify (* [path t] guaranteed to exist *)
    | Rename
    | Unknown
        (** multiple actions merged into one by debouncing or an uninformative
            "rename". inspect the FS to see what happened *)

  val dyn_of_action : action -> Dyn.t

  (** [action t] describes the action occured to [path t] *)
  val action : t -> action
end

(** the type of fsevents watcher *)
type t

(** [create ~paths ~latency ~f] create a new watcher watching [paths], with
    debouncing based on [latency]. [f] is called for every new event *)
val create : paths:string list -> latency:float -> f:(Event.t list -> unit) -> t

(** [start t] will start listening for fsevents. Note that the callback will not
    be called until [loop t] is called. *)
val start : t -> RunLoop.t -> unit

val runloop : t -> RunLoop.t option

(** [stop t] stop listening to events. Note that this will not make [loop]
    return until [break] is called. *)
val stop : t -> unit

(** [flush_sync t] flush all pending events that might be held up by debouncing.
    this function blocks until the final invocation of the callback for all
    buffered events completes. *)
val flush_sync : t -> unit

(** Set a list of directories to ignore. A maximum of 8 directories may be
    specified *)
val set_exclusion_paths : t -> paths:string list -> unit

(** [flush_async t] ask to flush buffered events but do not block. returns the
    id of the last event buffered (if it exists) *)
val flush_async : t -> [ `Last of Event.Id.t | `No_events_queued ]
