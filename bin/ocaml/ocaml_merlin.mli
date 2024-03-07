open Import

val command : unit Cmd.t

module Dump_dot_merlin : sig
  val command : unit Cmd.t
end

val group : unit Cmd.t
