(** Menhir rules *)

open Import

(** Generate the rules for a [(menhir ...)] stanza. *)
val gen_rules
  :  dir:Path.Build.t
  -> module_path:Module_name.t list
  -> Compilation_context.t
  -> Menhir_stanza.t
  -> unit Memo.t

val menhir_env : dir:Path.Build.t -> string list Action_builder.t Menhir_env.t Memo.t
