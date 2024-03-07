(** Generate module stanza *)

open! Stdune

(** create the rule and return the produced file *)
val setup_rules
  :  Super_context.t
  -> dir:Path.Build.t
  -> Generate_sites_module_stanza.t
  -> string Memo.t
