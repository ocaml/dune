type t

type dst =
  | Out of out_channel
  | Custom of
      { write : string -> unit
      ; close : unit -> unit
      }

val create : dst -> t

val emit : t -> Chrome_trace.Event.t -> unit

val record_gc_and_fd : t -> unit

val close : t -> unit
