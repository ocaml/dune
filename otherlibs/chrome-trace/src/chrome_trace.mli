[@@@alert
unstable "The API of this library is not stable and may change without notice."]

[@@@alert "-unstable"]

(** Output trace data to a file in Chrome's trace_event format. This format is
    compatible with chrome trace viewer [chrome://tracing].

    Trace viewer is a part of the catapult project
    (https://github.com/catapult-project/catapult/blob/master/tracing/README.md).

    The trace format is documented at:
    https://docs.google.com/document/d/1CvAClvFfyA5R-PhYUmn5OOQtYMH4h6I0nSsKchNAySU/preview *)

module Json : sig
  (** Simplifies JSON type *)
  type t =
    [ `Int of int
    | `Float of float
    | `String of string
    | `List of t list
    | `Bool of bool
    | `Assoc of (string * t) list
    ]
end

module Id : sig
  type t

  val create : [ `String of string | `Int of int ] -> t
end

module Stack_frame : sig
  module Raw : sig
    type t

    val create : string list -> t
  end

  type t

  val create : ?parent:Id.t -> name:string -> category:string -> unit -> t
end

module Event : sig
  type t

  module Timestamp : sig
    type t

    val of_float_seconds : float -> t

    val to_float_seconds : t -> float
  end

  type common_fields

  val common_fields :
       ?tts:Timestamp.t
    -> ?cname:string
    -> ?cat:string list
    -> ?pid:int
    -> ?tid:int
    -> ?stackframe:[ `Id of Id.t | `Raw of Stack_frame.Raw.t ]
    -> ts:Timestamp.t
    -> name:string
    -> unit
    -> common_fields

  val ts : common_fields -> Timestamp.t

  val set_ts : common_fields -> Timestamp.t -> common_fields

  type args = (string * Json.t) list

  (** Create a counter event *)
  val counter : ?id:Id.t -> common_fields -> args -> t

  type async =
    | Start
    | Instant
    | End

  val async : ?scope:string -> ?args:args -> Id.t -> async -> common_fields -> t

  val complete :
    ?tdur:Timestamp.t -> ?args:args -> dur:Timestamp.t -> common_fields -> t

  val to_json : t -> Json.t
end

module Output_object : sig
  (** The object format provided in whole *)

  type t

  val create :
       ?displayTimeUnit:[ `Ms | `Ns ]
    -> ?extra_fields:(string * Json.t) list
    -> ?stackFrames:(Id.t * Stack_frame.t) list
    -> traceEvents:Event.t list
    -> unit
    -> t

  val to_json : t -> Json.t
end
