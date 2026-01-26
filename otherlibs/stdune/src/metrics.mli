(** Utilities for collecting performance metrics *)

(** Reset all metrics to zero. *)
val reset : unit -> unit

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
