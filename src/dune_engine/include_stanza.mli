open Import

type context

val in_file : Path.Source.t -> context

val load_sexps :
  context:context -> Loc.t * string -> Dune_lang.Ast.t list * context
