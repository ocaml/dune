open Import

val exe_stanzas
  : Jbuild.Stanza.t list
  -> (Jbuild.Executables.t * Module.t String_map.t) option

val add_module_rules
  : Super_context.t
  -> dir:Path.t
  -> (unit, Lib.t list) Build.t
  -> unit

val target
  : (Path.t * Jbuild.Scope.t * Jbuild.Stanzas.t) list
  -> string
  -> Path.t option
