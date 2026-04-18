open Import

type command =
  { prog : string
  ; argv : string list
  }

val wrap : cwd:string -> string list -> command

module With_bwrap : sig
  val command : unit Cmd.t
end
