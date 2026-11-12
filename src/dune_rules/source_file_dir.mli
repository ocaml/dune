open Import

type t =
  { dir : Path.Build.t
  ; path_to_root : Filename.t list
  ; files : Filename.Array.Set.t
  ; source_dir : Source_tree.Rules.Dir.t option
  ; stanzas : Stanza.t list
  }

val materialize
  :  dst:Path.Build.t
  -> Source_tree.Rules.File.materialization
  -> Action.Full.t Action_builder.With_targets.t

val rules : dir:Path.Build.t -> Source_tree.Rules.Dir.t -> Rule.t Memo.t Filename.Map.t
