(** Simple rules: user, copy_files, alias *)

open Import

module Alias_rules : sig
  val add
    :  Super_context.t
    -> aliases:Alias.t list
    -> loc:Loc.t
    -> Action.Full.t Action_builder.t
    -> unit Memo.t

  val add_empty
    :  Super_context.t
    -> loc:Stdune.Loc.t
    -> aliases:Alias.t list
    -> unit Memo.t
end

(** Interpret a [(rule ...)] stanza and return the targets it produces, if any. *)
val user_rule
  :  Super_context.t
  -> dir:Path.Build.t
  -> expander:Expander.t
  -> Rule_conf.t
  -> Targets.Validated.t option Memo.t

(** Where the relative glob of a [(copy_files ...)] stanza in [dir] points: a
    workspace or external path for a workspace directory, a build path inside
    the package for a mounted one. *)
val copy_files_relative_glob : dir:Path.Build.t -> loc:Loc.t -> string -> Path.t

(** Interpret a [(copy_files ...)] stanza and return the targets it produces. *)
val copy_files
  :  Super_context.t
  -> dir:Path.Build.t
  -> expander:Expander.t
  -> Copy_files.t
  -> Path.Set.t Memo.t

(** Interpret an [(alias ...)] stanza. *)
val alias
  :  Super_context.t
  -> dir:Path.Build.t
  -> expander:Expander.t
  -> Alias_conf.t
  -> unit Memo.t

val interpret_and_add_locks
  :  expander:Expander.t
  -> Locks.t
  -> Action.Full.t Action_builder.t
  -> Action.Full.t Action_builder.t
