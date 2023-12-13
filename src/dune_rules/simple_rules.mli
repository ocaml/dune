(** Simple rules: user, copy_files, alias *)

open Import

module Alias_rules : sig
  val add
    :  Super_context.t
    -> alias:Alias.t
    -> loc:Loc.t
    -> Action.Full.t Action_builder.t
    -> unit Memo.t

  val add_empty : Super_context.t -> loc:Stdune.Loc.t -> alias:Alias.t -> unit Memo.t
end

(** Interpret a [(rule ...)] stanza and return the targets it produces, if any. *)
val user_rule
  :  Super_context.t
  -> ?extra_bindings:Value.t list Pform.Map.t
  -> dir:Path.Build.t
  -> expander:Expander.t
  -> Rule_conf.t
  -> Targets.Validated.t option Memo.t

(** Interpret a [(copy_files ...)] stanza and return the targets it produces. *)
val copy_files
  :  Super_context.t
  -> dir:Path.Build.t
  -> expander:Expander.t
  -> src_dir:Path.Source.t
  -> Copy_files.t
  -> Path.Set.t Memo.t

(** Interpret an [(alias ...)] stanza. *)
val alias
  :  Super_context.t
  -> ?extra_bindings:Value.t list Pform.Map.t
  -> dir:Path.Build.t
  -> expander:Expander.t
  -> Alias_conf.t
  -> unit Memo.t
