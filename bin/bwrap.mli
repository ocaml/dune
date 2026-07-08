open Import

type command =
  { prog : string
  ; argv : string list
  }

val available : unit -> bool
val unavailable_reason : unit -> User_message.Style.t Pp.t list
val wrap : cwd:string -> string list -> command

module With_bwrap : sig
  val command : unit Cmd.t
end
