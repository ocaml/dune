(** Utop rules *)

open! Stdune

(** Return the name of the utop target inside a directory where some libraries
  are defined. *)
val utop_exe : string

val is_utop_dir : Path.Build.t -> bool

val setup : Super_context.t -> dir:Path.Build.t -> unit
