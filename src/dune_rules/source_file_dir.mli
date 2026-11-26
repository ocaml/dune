open Import

type t =
  { dir : Path.Build.t
  ; path_to_root : Filename.t list
  ; files : Filename.Array.Set.t
  ; source_dir : Source_tree.Dir.t option
  ; stanzas : Stanza.t list
  }

val rules : dir:Path.Build.t -> Source_tree.Dir.t -> Rule.t Filename.Map.t

(** The rule copying one source file that is not in the workspace. *)
val copy_rule : src:Path.t -> dst:Path.Build.t -> Rule.t
