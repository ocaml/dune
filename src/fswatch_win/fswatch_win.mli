(** File-watching support under Windows *)

module Event : sig
  (** The type of events *)
  type action =
    | Added (** The file was added. *)
    | Removed (** The file was removed. *)
    | Modified (** The file was modified. *)
    | Renamed_old (** The file was renamed. This corresponds to the old name. *)
    | Renamed_new (** The file was renamed. This corresponds to the new name. *)

  type t

  (** The directory being watched. *)
  val directory : t -> string

  (** The path to the file relevant to the event. Relative to the directory
      being watched (see {!directory}). *)
  val path : t -> string

  (** The description of the event action. *)
  val action : t -> action

  (** For debugging. *)
  val to_dyn : t -> Dyn.t
end

(** The type of file watchers. Each file watcher can watch an arbitrary
    collection of directories. Multiple file watchers can be used
    simultaneously, if needed. *)
type t

(** Create a file watcher. This creates a native thread that will monitor for
    changes in the background. *)
val create : unit -> t

(** Start watching a directory for changes. The watching is recursive: all
    subdirectories are watched as well. Watching a single file is not possible. *)
val add : t -> string -> unit

(** Wait for events. This function will block until it receives some file change
    notifications. After it receives a notification, it will wait for [sleep]
    milliseconds before retrieving them and returning them to the user. This is
    done to avoid triggering multiple rebuilds in close succession. *)
val wait : t -> sleep:int -> Event.t list

(** Shutdown the file watcher. This tears down the background thread and frees
    all allocated resources. It is an error to call [add] or [wait] after this
    function returns. *)
val shutdown : t -> unit
