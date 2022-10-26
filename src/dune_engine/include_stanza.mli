open Import

val syntax : Dune_lang.Syntax.t

type 'path context

val get_current_file : 'path context -> 'path

val in_file : 'path -> 'path context

(** Returns [None] if the (generated) include can't yet be processed. *)
val load_sexps :
     context:Path.Source.t context
  -> generation_authorized:bool
  -> Loc.t * string
  -> (Dune_lang.Ast.t list * Path.Source.t context) option Memo.t

val load_sexps_generated :
     read_file:
       (   Path.t
        -> f:(Path.t -> Dune_lang.Ast.t list)
        -> Dune_lang.Ast.t list Memo.t)
  -> file_exists:(Path.Source.t -> bool Memo.t)
  -> context:Path.Build.t context
  -> Loc.t * string
  -> (Dune_lang.Ast.t list * Path.Build.t context) Memo.t
