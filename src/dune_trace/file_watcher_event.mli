open Stdune

type kind =
  | Created
  | Deleted
  | File_changed
  | Unknown

type t =
  [ `File of Path.t * kind
  | `Queue_overflow
  | `Sync of int
  | `Watcher_terminated
  ]

val kind_repr : kind Repr.t
val to_event : t -> Event.t
