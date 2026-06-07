open Import

val melc : Super_context.t -> loc:Loc.t option -> dir:Path.Build.t -> Action.Prog.t Memo.t
val available : Super_context.t -> dir:Path.Build.t -> bool Memo.t

val effective_lib_modes
  :  Super_context.t
  -> dir:Path.Build.t
  -> Lib_mode.Map.Set.t
  -> Lib_mode.Map.Set.t Memo.t

val where : Super_context.t -> loc:Loc.t option -> dir:Path.Build.t -> Path.t list Memo.t
