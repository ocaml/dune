open Import

type command =
  { prog : string
  ; argv : string list
  }

val wrap : cwd:string -> string list -> command

module With_bwrap : sig
  val command : unit Cmd.t
end

module With_sandbox_exec : sig
  val command : unit Cmd.t
end
