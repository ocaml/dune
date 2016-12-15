val gen
  :  context:Context.t
  -> tree:Alias.tree
  -> stanzas:(Path.t * Jbuild_types.Stanza.t list) list
  -> packages:string list
  -> ?filter_out_optional_stanzas_with_missing_deps:bool (** default: true *)
  -> unit
  -> (unit, unit) Build.t list
