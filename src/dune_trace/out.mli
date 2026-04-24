open Stdune

type t =
  { fd : Fd.t
  ; buf : Buffer.t
  ; cats : Category.Set.t
  ; mutex : Mutex.t
  ; path : Stdune.Path.t option
  ; terminate_process_on_error : bool
  }

val emit : ?buffered:bool -> t -> Event.t -> unit
val flush : t -> unit
val close : t -> unit
val create : Category.t list -> Stdune.Path.t -> t
val of_fd : Category.t list -> Unix.file_descr -> t
val start : t option -> (unit -> Event.Async.data) -> Event.Async.t option
val finish : t -> Event.Async.t option -> unit
