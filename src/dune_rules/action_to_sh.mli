(** Convert an action to a shell command suitable for [/bin/sh] *)
open! Build_api.Api

open Stdune

val pp : Action.For_shell.t -> unit Pp.t
