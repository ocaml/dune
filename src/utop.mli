open Import

val exe_stanzas
  : Jbuild.Stanza.t list
  -> (Jbuild.Executables.t * Module.t String_map.t) option

val add_module_rules
  : Super_context.t
  -> dir:Path.t
  -> (unit, Lib.t list) Build.t
  -> unit

val src_dir : Path.t -> Path.t

val target : Context.t -> Path.t
