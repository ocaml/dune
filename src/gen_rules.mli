open Import

val gen
  :  context:Context.t
  -> file_tree:File_tree.t
  -> tree:Alias.tree
  -> stanzas:(Path.t * Jbuild_types.Stanza.t list) list
  -> packages:Package.t String_map.t
  -> ?filter_out_optional_stanzas_with_missing_deps:bool (** default: true *)
  -> unit
  -> Build_interpret.Rule.t list
