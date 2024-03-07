(** Loading build rules *)

open Import

(** A way to determine the [Loc.t] of the current rule. Set by [Build_system]. *)
val set_current_rule_loc : (unit -> Loc.t option Memo.t) -> unit

module Loaded : sig
  type rules_here =
    { by_file_targets : Rule.t Path.Build.Map.t
    ; by_directory_targets : Rule.t Path.Build.Map.t
    }

  val no_rules_here : rules_here

  type build =
    { allowed_subdirs : Path.Unspecified.w Dir_set.t
    ; rules_here : rules_here
    ; aliases : (Loc.t * Rules.Dir_rules.Alias_spec.item) list Alias.Name.Map.t
    }

  (* CR-someday amokhov: Switch to [Filename_set.Source.t] and [Filename_set.External.t]
     or something similar to avoid handling filename sets unanchored to their directory. *)
  type t =
    | Source of { filenames : Filename.Set.t }
    | External of { filenames : Filename.Set.t }
    | Build of build
    | Build_under_directory_target of { directory_target_ancestor : Path.Build.t }

  val no_rules : allowed_subdirs:Path.Unspecified.w Dir_set.t -> t
end

(** Load the rules for this directory. *)
val load_dir : dir:Path.t -> Loaded.t Memo.t

(** Return the rule that has the given file has target, if any *)
val get_rule : Path.t -> Rule.t option Memo.t

(** Return the definition of an alias. *)
val get_alias_definition
  :  Alias.t
  -> (Loc.t * Rules.Dir_rules.Alias_spec.item) list Memo.t

type target_type =
  | File
  | Directory

type is_target =
  | No
  | Yes of target_type
  | Under_directory_target_so_cannot_say

val is_target : Path.t -> is_target Memo.t

(** [is_under_directory_target p] returns [true] iff [p] is a descendant of one.
    Returns [true] if [p] is a directory target itself.

    This is similar to:

    {[
      is_target p
      >>= function
      | No | Yes File -> false
      | Yes Directory | under_directory_target_so_cannot_say -> true
    ]}

    Except that it forces less rules to be computed, thus creating less
    opportunities for creating computation cycles. *)
val is_under_directory_target : Path.t -> bool Memo.t

type rule_or_source =
  | Source of Digest.t
  | Rule of Path.Build.t * Rule.t

val get_rule_or_source : Path.t -> rule_or_source Memo.t
