(** Generate preprocessed source files in root/ppx *)

open Import

type t = { dirs_to_exclude : String_with_vars.t list }

include Stanza.S with type t := t

val gen_stanza_rules
  :  dir:Path.Build.t
  -> dirs_to_exclude:Path.t list
  -> Super_context.t
  -> unit Memo.t

val gen_sub_dir_rules : dir:Path.Build.t -> Build_config.Gen_rules.t Memo.t
