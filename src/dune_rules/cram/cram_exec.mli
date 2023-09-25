open Import

type ('path, _) spec =
  { script : 'path
  ; shell_prog : 'path
  ; shell_args : string list
  }

val action : (Path.t, Import.Path.Build.t) spec -> Action.t
