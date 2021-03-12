(** Output trace data to a file in Chrome's trace_event format. This format is
    compatible with chrome trace viewer [chrome://tracing].

    Trace viewer is a part of the catapult project
    (https://github.com/catapult-project/catapult/blob/master/tracing/README.md).

    The trace format is documented at:
    https://docs.google.com/document/d/1CvAClvFfyA5R-PhYUmn5OOQtYMH4h6I0nSsKchNAySU/preview *)

module Json : sig
  (** Simplifies JSON type *)
  type t =
    | Int of int
    | Float of float
    | String of string
    | Array of t list
    | Bool of bool
    | Object of (string * t) list

  val to_string : t -> string
end

module Event : sig
  type t

  module Timestamp : sig
    type t

    val of_float_seconds : float -> t

    val now : unit -> t
  end

  module Id : sig
    type t =
      | Int of int
      | String of string
  end

  type common

  val common :
       ?tts:Timestamp.t
    -> ?cname:string
    -> ?cat:string list
    -> ?pid:int
    -> ?tid:int
    -> ts:Timestamp.t
    -> name:string
    -> unit
    -> common

  val set_ts : common -> Timestamp.t -> common

  type args = (string * Json.t) list

  (** Create a counter event *)
  val counter : ?id:Id.t -> common -> args -> t

  type async =
    | Start
    | Instant
    | End

  val async : ?scope:string -> ?args:args -> Id.t -> async -> common -> t

  val complete :
    ?tdur:Timestamp.t -> ?args:args -> dur:Timestamp.t -> common -> t

  val to_json : t -> Json.t
end
