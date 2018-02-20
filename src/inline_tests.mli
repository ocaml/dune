open Import

val setup
  :  Super_context.t
  -> dir:Path.t
  -> lib:Jbuild.Library.t
  -> scope:Scope.t
  -> modules:Module.t String_map.t
  -> compile_info:Lib.Compile.t
  -> unit
