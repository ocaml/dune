open Import

type rule =
  { exe: Jbuild.Executables.t
  ; alias_name: string
  ; alias_action: (unit, Action.t) Build.t
  ; alias_stamp: Sexp.t
  ; all_modules: Module.t String_map.t
  ; gen_source : (unit, Action.t) Build.t
  }

val rule
  : Super_context.t
  -> dir:Path.t
  -> lib:Jbuild.Library.t
  -> scope:Lib_db.Scope.t With_required_by.t
  -> modules:Module.t String_map.t
  -> rule option
