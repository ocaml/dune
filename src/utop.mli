(** Utop rules *)

open! Stdune

val utop_exe : Path.t -> Path.t
(** Return the path of the utop bytecode binary inside a directory where
    some libraries are defined. *)

val is_utop_dir : Path.t -> bool

val setup : Super_context.t -> dir:Path.t -> unit
