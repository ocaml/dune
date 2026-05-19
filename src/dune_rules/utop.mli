(** Utop rules *)

open Import

(** Path of the utop executable target relative to the directory where utop
    rules are generated. *)
val utop_exe : string

val utop_dir_basename : Filename.t

(** Path of the generated findlib configuration file relative to the directory
    where utop rules are generated. *)
val utop_findlib_conf : string

val utop_dev_tool_lock_dir_exists : bool Memo.Lazy.t
val libs_under_dir : Super_context.t -> db:Lib.DB.t -> dir:Path.t -> Lib.t list Memo.t
val setup : Super_context.t -> dir:Path.Build.t -> unit Memo.t

val requires_under_dir
  :  Super_context.t
  -> dir:Path.Build.t
  -> Lib.t list Resolve.t Memo.t
