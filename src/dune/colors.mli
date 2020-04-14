open! Stdune

val setup_env_for_colors : Env.t -> Env.t
(** [Env.initial] extended with variables to force a few tools to print colors *)

val setup_err_formatter_colors : unit -> unit
(** Enable the interpretation of color tags for [Format.err_formatter] *)
