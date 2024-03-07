open! Stdune
module Inotify_lib := Async_inotify_for_dune.Async_inotify

type t

module Fs_memo_event : sig
  (* Here are some idealized assumptions the Fs_memo module in dune_engine makes
     about events:

     - If a file is renamed, we receive [Created] and [Deleted] events with
       corresponding paths.

     - If a directory is renamed then in addition to the [Created] and [Deleted]
       events for the directory itself, we receive events about all file and
       directory paths in the corresponding file tree.

     - Similarly, if a directory is deleted, we receive the [Deleted] event for
       the directory itself, as well as deletion events for all watched paths in
       the corresponding file tree.

     Not all of these assumptions we can currently uphold. In particular,
     directory renames probably just give "created" and "deleted" for the
     directory itself, which means we are not correctly handling directory
     renames. *)
  type kind =
    | Created
    | Deleted
    | File_changed
    | Unknown (** Treated conservatively as any possible event. *)

  type t = private
    { path : Path.t
    ; kind : kind
    }

  val to_dyn : t -> Dyn.t
end

module Sync_id : Id.S

module Event : sig
  type t =
    | Fs_memo_event of Fs_memo_event.t
    | Queue_overflow
    | Sync of Sync_id.t
    | Watcher_terminated
end

module Scheduler : sig
  (** Hook into the fiber scheduler. *)
  type t =
    { spawn_thread : (unit -> unit) -> unit
    (** We spawn threads through this function in case the scheduler wants
        to block signals *)
    ; thread_safe_send_emit_events_job : (unit -> Event.t list) -> unit
    (** Send some events to the scheduler. The events are sent in the form
        of a thunk to be executed on the scheduler thread, so that we can
        do some bookkeeping that needs to happen there. *)
    }
end

(** Create a new file watcher with default settings. *)
val create_default
  :  ?fsevents_debounce:float
  -> watch_exclusions:string list
  -> scheduler:Scheduler.t
  -> unit
  -> t

(** The action that needs to be taken to shutdown the watcher. *)
val shutdown : t -> [ `Kill of Pid.t | `No_op | `Thunk of unit -> unit ]

val wait_for_initial_watches_established_blocking : t -> unit

(** Cause a [Sync] event to be propagated through the notification subsystem to
    attempt to make sure that we've processed all the events that happened so
    far. *)
val emit_sync : t -> Sync_id.t

val add_watch : t -> Path.t -> (unit, [ `Does_not_exist ]) result

module For_tests : sig
  val should_exclude : watch_exclusions:string list -> string -> bool
end
