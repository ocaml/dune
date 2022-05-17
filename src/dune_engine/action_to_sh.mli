(** Convert an action to a shell command suitable for [/bin/sh] *)

val pp : Action.For_shell.t -> _ Pp.t
