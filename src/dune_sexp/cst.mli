(** Concrete syntax tree *)

open Stdune

type t =
  | Atom of Loc.t * Atom.t
  | Quoted_string of Loc.t * string
  | Template of Template.t
  | List of Loc.t * t list
  | Comment of Loc.t * string list

val loc : t -> Loc.t
val abstract : t -> Ast.t option
val concrete : Ast.t -> t
val to_dyn : t -> Dyn.t
val to_sexp : t -> T.t option

(** Return all the comments contained in a concrete syntax tree *)
val extract_comments : t list -> (Loc.t * string list) list

val tokenize : t list -> (Loc.t * Lexer.Token.t) list
