val gen
  :  context:Context.t
  -> tree:Alias.tree
  -> stanzas:(Path.t * Jbuild_types.Stanza.t list) list
  -> packages:string list
  -> (unit, unit) Build.t list
