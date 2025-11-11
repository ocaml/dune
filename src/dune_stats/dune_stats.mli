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

type event

type event_data =
  { args : Chrome_trace.Event.args
  ; cat : string list
  ; name : string
  }

val start : t option -> (unit -> event_data) -> event option
val finish : event option -> unit
val flush : t -> unit

module Not_a_fiber : sig
  (** Please make sure what you are wrapping is not a fiber. *)

  module O : sig
    val ( let& ) : event_data -> (unit -> 'a) -> 'a
  end
end

module Fiber : sig
  module O : sig
    val ( let& ) : event_data -> (unit -> 'a Fiber.t) -> 'a Fiber.t
  end
end

module Memo : sig
  module O : sig
    val ( let& ) : event_data -> (unit -> 'a Memo.t) -> 'a Memo.t
  end
end

module Private : sig
  module Fd_count : sig
    type t =
      | Unknown
      | This of int

    val get : unit -> t
  end
end
