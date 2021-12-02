(** Utop rules *)
open! Dune_engine

open! Stdune

(** Return the name of the utop target inside a directory where some libraries
    are defined. *)
val utop_exe : string

val utop_dir_basename : string

val libs_under_dir :
  Super_context.t -> db:Lib.DB.t -> dir:Path.t -> Lib.L.t Memo.Build.t

val setup : Super_context.t -> dir:Path.Build.t -> unit Memo.Build.t
