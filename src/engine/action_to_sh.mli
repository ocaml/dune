(** Convert an action to a shell command suitable for [/bin/sh] *)

open Stdune
open Dune_rules

val pp : Action.For_shell.t -> unit Pp.t
