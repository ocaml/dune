(** Loading build rules *)

open! Stdune
open! Import
module Action_builder := Action_builder0

(** A way to determine the [Loc.t] of the current rule. Set by [Build_system]. *)
val set_current_rule_loc : (unit -> Loc.t option Memo.Build.t) -> unit

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

  type t =
    | Non_build of Path.Set.t
    | Build of build

  val no_rules : allowed_subdirs:Path.Unspecified.w Dir_set.t -> t
end

(** Load the rules for this directory. *)
val load_dir : dir:Path.t -> Loaded.t Memo.Build.t

val alias_exists : Alias.t -> bool Memo.Build.t

(** Return the rule that has the given file has target, if any *)
val get_rule : Path.t -> Rule.t option Memo.Build.t

(** Return the definition of an alias. *)
val get_alias_definition :
  Alias.t -> (Loc.t * Rules.Dir_rules.Alias_spec.item) list Memo.Build.t

type target_type =
  | File
  | Directory

type is_target =
  | No
  | Yes of target_type
  | Under_directory_target_so_cannot_say

val is_target : Path.t -> is_target Memo.Build.t

(** List of all buildable direct targets. This does not include files and
    directory produced under a directory target. *)
val all_direct_targets : unit -> target_type Path.Build.Map.t Memo.Build.t

type rule_or_source =
  | Source of Digest.t
  | Rule of Path.Build.t * Rule.t

val get_rule_or_source : Path.t -> rule_or_source Memo.Build.t
