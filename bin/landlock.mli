open Import

type command =
  { prog : string
  ; argv : string list
  }

val available : unit -> bool
val wrap : dune_prog:Path.t -> string list -> command option
val wrap_exn : dune_prog:Path.t -> string list -> command

module With_landlock : sig
  val command : unit Cmd.t
end
