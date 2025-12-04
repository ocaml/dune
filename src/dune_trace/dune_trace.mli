type t

module Json : sig
  val to_string : Chrome_trace.Json.t -> string
end

type dst =
  | Out of out_channel
  | Custom of
      { write : string -> unit
      ; close : unit -> unit
      ; flush : unit -> unit
      }

val global : unit -> t option
val set_global : t -> unit
val create : extended_build_job_info:bool -> dst -> t
val emit : t -> Chrome_trace.Event.t -> unit
val record_gc_and_fd : t -> unit
val close : t -> unit
val extended_build_job_info : t -> bool

module Event : sig
  type t

  type data =
    { args : Chrome_trace.Event.args option
    ; cat : string list option
    ; name : string
    }
end

val start : t option -> (unit -> Event.data) -> Event.t option
val finish : Event.t option -> unit
val flush : t -> unit

module Private : sig
  module Fd_count : sig
    type t =
      | Unknown
      | This of int

    val get : unit -> t
  end
end
