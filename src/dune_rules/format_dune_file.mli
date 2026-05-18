open Import

val action : version:Dune_lang.Syntax.Version.t -> Path.t -> Path.Build.t -> Action.t
val action_stdout : version:Dune_lang.Syntax.Version.t -> Path.t -> Action.t
