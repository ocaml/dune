open Stdune

type t

val fd : t -> Fd.t
val cats : t -> Category.Set.t
val alloc : t -> Alloc.t option
val emit : ?buffered:bool -> t -> Event.t -> unit
val flush : t -> unit
val close : t -> unit
val create : Category.t list -> Stdune.Path.t -> t
val of_fd : Category.t list -> Fd.t -> t
val start : t option -> (unit -> Event.Async.data) -> Event.Async.t option
val finish : t -> Event.Async.t option -> unit
