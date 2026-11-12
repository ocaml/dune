open Stdune

type kind =
  | Created
  | Deleted
  | File_changed
  | Unknown

type sync =
  { id : int
  ; retries : int
  }

type t =
  [ `File of Path.t * kind
  | `Queue_overflow
  | `Sync of sync
  | `Watcher_terminated
  ]

val kind_repr : kind Repr.t
val sync_repr : sync Repr.t
val to_event : t -> Event.t
val debounce_extend : files:(Path.t * kind) list -> invalidation_empty:bool -> Event.t
