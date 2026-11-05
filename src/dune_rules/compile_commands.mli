open Import

(** Set up lazy workspace aliases for compilation database generation. *)
val setup_aliases : Super_context.t -> unit Memo.t

(** Generate the internal, promoted compilation database rule. *)
val gen_rules : Super_context.t Memo.t -> dir:Path.Build.t -> unit Memo.t
