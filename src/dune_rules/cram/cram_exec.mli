open Import

type ('path, _) spec =
  { script : 'path
  ; shell : 'path
  }

val action : (Path.t, Import.Path.Build.t) spec -> Action.t
