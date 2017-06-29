
val add_rules
  : Super_context.t
  -> dir:Path.t
  -> Jbuild.Stanza.t list
  -> (Jbuild.Executables.t * Module.t Import.String_map.t) option

val target
  : (Path.t * Jbuild.Scope.t * Jbuild.Stanzas.t) list
  -> string
  -> Path.t option
