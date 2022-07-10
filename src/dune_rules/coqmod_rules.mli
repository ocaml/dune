open Import

val deps_of : Coq_module.t -> Coqmod.t Action_builder.t

val add_rule : Super_context.t -> Coq_module.t -> unit Memo.t
