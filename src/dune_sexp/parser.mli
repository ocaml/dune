(** Parsing of the Dune language *)

open Stdune

module Mode : sig
  type 'a t =
    | Single : Ast.t t
    | Many : Ast.t list t
    | Many_as_one : Ast.t t
    | Cst : Cst.t list t
end

val parse : mode:'a Mode.t -> ?lexer:Lexer.t -> Lexing.lexbuf -> 'a
val parse_string : fname:string -> mode:'a Mode.t -> ?lexer:Lexer.t -> string -> 'a
val load : ?lexer:Lexer.t -> Path.t -> mode:'a Mode.t -> 'a

(** Insert comments in a concrete syntax tree. Comments are inserted based on
    their location. *)
val insert_comments : Cst.t list -> (Loc.t * string list) list -> Cst.t list
