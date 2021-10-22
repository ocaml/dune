open Import

type 'path context

val get_current_file : 'path context -> 'path

val in_file : 'path -> 'path context

val load_sexps :
     context:Path.Source.t context
  -> Loc.t * string
  -> Dune_lang.Ast.t list * Path.Source.t context

val get_include_path_generated :
  context:Path.Build.t context -> Loc.t * string -> Path.Build.t context

val load_sexps_generated : context:Path.Build.t context -> Dune_lang.Ast.t list

val load_sexps_source :
  loc:Loc.t -> context:Path.Build.t context -> Dune_lang.Ast.t list
