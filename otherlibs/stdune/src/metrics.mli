(** Utilities for collecting performance metrics *)

(** Reset all metrics to zero. *)
val reset : unit -> unit

module Build : sig
  val add_process_times
    :  elapsed_time:Time.Span.t
    -> user_cpu_time:Time.Span.t option
    -> system_cpu_time:Time.Span.t option
    -> unit

  val process_count : unit -> int
  val process_time : unit -> Time.Span.t
  val process_user_cpu_time : unit -> Time.Span.t
  val process_system_cpu_time : unit -> Time.Span.t
  val reset : unit -> unit
end

module type Stat = sig
  val count : Counter.t
  val bytes : Counter.t
  val time : Counter.Timer.t
end

module File_read : Stat
module File_write : Stat

module Directory_read : sig
  val count : Counter.t
  val time : Counter.Timer.t
end

module Digest : sig
  module Value : Stat
  module File : Stat
end
