(** Utop rules *)
open! Dune_engine

open! Stdune

(** Return the name of the utop target inside a directory where some libraries
    are defined. *)
val utop_exe : string

val is_utop_dir : Path.Build.t -> bool

val libs_under_dir : Super_context.t -> db:Lib.DB.t -> dir:Path.t -> Lib.L.t

val setup : Super_context.t -> dir:Path.Build.t -> unit
