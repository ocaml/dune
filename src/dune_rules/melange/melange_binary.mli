open Import

val melc : Super_context.t -> loc:Loc.t option -> dir:Path.Build.t -> Action.Prog.t Memo.t

val melobjinfo
  :  Super_context.t
  -> loc:Loc.t option
  -> dir:Path.Build.t
  -> Action.Prog.t Memo.t

val available : Super_context.t -> dir:Path.Build.t -> bool Memo.t

(** Runtime defaults for source compilation, preserving inherited settings. *)
val compilation_env_overrides
  :  Super_context.t
  -> dir:Path.Build.t
  -> Action.Prog.t
  -> Env.t Memo.t

val where : Super_context.t -> loc:Loc.t option -> dir:Path.Build.t -> Path.t list Memo.t
