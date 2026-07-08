open Import

val available : unit -> bool

module With_landlock : sig
  val command : unit Cmd.t
end
