type t =
  { fd : Unix.file_descr
  ; buf : Buffer.t
  ; cats : Category.Set.t
  ; mutex : Mutex.t
  }

val emit : t -> Event.t -> unit
val close : t -> unit
val create : Category.t list -> Stdune.Path.t -> t
val start : t option -> (unit -> Event.Async.data) -> Event.Async.t option
val finish : t -> Event.Async.t option -> unit
