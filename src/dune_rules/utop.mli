(** Utop rules *)

open Import

(** Return the name of the utop target inside a directory where some libraries
    are defined. *)
val utop_exe : string

val utop_dir_basename : string

val libs_under_dir :
  Super_context.t -> db:Lib.DB.t -> dir:Path.t -> Lib.L.t Memo.t

val setup : Super_context.t -> dir:Path.Build.t -> unit Memo.t
