open Import

val melc : Super_context.t -> loc:Loc.t option -> dir:Path.Build.t -> Action.Prog.t Memo.t

val melobjinfo
  :  Super_context.t
  -> loc:Loc.t option
  -> dir:Path.Build.t
  -> Action.Prog.t Memo.t

val available : Super_context.t -> dir:Path.Build.t -> bool Memo.t

(** Probe the linked runtime, which can differ from Melange's compiler version. *)
val supports_small_heap_limit : Path.t -> dir:Path.Build.t -> env:Env.t -> bool Memo.t

val where : Super_context.t -> loc:Loc.t option -> dir:Path.Build.t -> Path.t list Memo.t
