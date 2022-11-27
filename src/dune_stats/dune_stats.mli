type t

module Json : sig
  val to_string : Chrome_trace.Json.t -> string
end

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

type event

type event_data =
  { args : Chrome_trace.Event.args option
  ; cat : string list option
  ; name : string
  }

val start : t option -> (unit -> event_data) -> event option

val finish : event option -> unit

module Private : sig
  module Fd_count : sig
    type t =
      | Unknown
      | This of int

    val get : unit -> t
  end
end
