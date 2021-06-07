(** An [Lwt] wrapper for {!Inotify} module *)

(** Type of inotify descriptors. *)
type t

(** [create ()] returns a new inotify descriptor. *)
val create    : unit -> t Lwt.t

(** [add_watch desc path events] sets up [desc] to watch for [events] occuring
    to [path], and returns a watch descriptor. *)
val add_watch : t -> string -> Inotify.selector list -> Inotify.watch Lwt.t

(** [rm_watch desc watch] stops [desc] from watching [watch]. *)
val rm_watch  : t -> Inotify.watch -> unit Lwt.t

(** [read desc] waits for an event to occur at [desc]. *)
val read      : t -> Inotify.event Lwt.t

(** [try_read desc] returns [Some event] if [desc] has queued events,
    or [None] otherwise. *)
val try_read  : t -> Inotify.event option Lwt.t

(** [close desc] frees [desc]. *)
val close     : t -> unit Lwt.t
