open Import

type t =
  { dir : Path.Build.t
  ; path_to_root : Filename.t list
  ; files : Filename.Array.Set.t
  ; source_dir : Source_tree.Rules.Dir.t option
  ; stanzas : Stanza.t list
  }
