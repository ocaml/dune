
type conf =
  { tree     : Alias.tree
  ; stanzas  : (Path.t * Jbuild_types.Stanza.t list) list
  ; packages : string list
  }

val load : unit -> conf
