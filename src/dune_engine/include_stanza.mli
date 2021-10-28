open Import

type 'path context

val get_current_file : 'path context -> 'path

val in_file : 'path -> 'path context

(** Return None if the include can't be yet processed (generated include) *)
val load_sexps :
     context:Path.Source.t context
  -> generation_authorized:bool
  -> Loc.t * string
  -> (Dune_lang.Ast.t list * Path.Source.t context) option

val load_sexps_generated :
     read_file:
       (   Path.t
        -> f:(Path.t -> Dune_lang.Ast.t list)
        -> Dune_lang.Ast.t list Memo.Build.t)
  -> file_exists:(Path.Source.t -> bool Memo.Build.t)
  -> context:Path.Build.t context
  -> Loc.t * string
  -> (Dune_lang.Ast.t list * Path.Build.t context) Memo.Build.t

val syntax : Dune_lang.Syntax.t
