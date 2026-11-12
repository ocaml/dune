open Import

type 'a context

val in_source_file : Source_tree_file.File.t -> Source_tree_file.File.t context
val in_src_file : Path.Source.t -> Source_tree_file.File.t context
val in_build_file : Path.Build.t -> Path.Build.t context
val file_path : 'a context -> Loc.t -> string -> 'a

val load_sexps
  :  context:'a context
  -> Loc.t * string
  -> (Dune_lang.Ast.t list * 'a context) Memo.t
