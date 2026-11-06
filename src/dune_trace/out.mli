open Stdune

type t

val duplicate_fd : t -> Fd.t option
val cats : t -> Category.Set.t
val alloc : t -> Alloc.t option
val emit : ?buffered:bool -> t -> Event.t -> unit
val flush : t -> unit
val close : t -> Unix.error option
val create : [ `Path of Stdune.Path.t | `Fd of Fd.t ] -> t
val emit_runtime : t -> unit
val start : t option -> (unit -> Event.Async.data) -> Event.Async.t option
val finish : t -> Event.Async.t option -> unit
