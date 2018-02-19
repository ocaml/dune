open Import

val setup
  :  Super_context.t
  -> dir:Path.t
  -> lib:Jbuild.Library.t
  -> scope:Lib_db.Scope.t With_required_by.t
  -> modules:Module.t String_map.t
  -> unit
