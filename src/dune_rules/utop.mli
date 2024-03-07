(** Utop rules *)

open Import

(** Return the name of the utop target inside a directory where some libraries
    are defined. *)
val utop_exe : Filename.t

val utop_dir_basename : Filename.t
val libs_under_dir : Super_context.t -> db:Lib.DB.t -> dir:Path.t -> Lib.t list Memo.t
val setup : Super_context.t -> dir:Path.Build.t -> unit Memo.t

val requires_under_dir
  :  Super_context.t
  -> dir:Path.Build.t
  -> Lib.t list Resolve.t Memo.t
