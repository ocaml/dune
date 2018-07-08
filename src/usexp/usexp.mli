(** Parsing of s-expressions.

    This library is internal to jbuilder and guarantees no API stability.*)

type syntax = Jbuild | Dune

module Atom : sig
  type t = private A of string [@@unboxed]

  val is_valid : t -> syntax -> bool

  val of_string : string -> t
  val to_string : t -> string

  val of_int : int -> t
  val of_float : float -> t
  val of_bool : bool -> t
  val of_int64 : Int64.t -> t
  val of_digest : Digest.t -> t
end

module Loc : sig
  type t =
    { start : Lexing.position
    ; stop  : Lexing.position
    }

  val in_file : string -> t

  val none : t

  val of_lexbuf : Lexing.lexbuf -> t
end

module Template : sig
  type var_syntax = Dollar_brace | Dollar_paren | Percent

  type var =
    { loc: Loc.t
    ; name: string
    ; payload: string option
    ; syntax: var_syntax
    }

  type part =
    | Text of string
    | Var of var

  type t =
    { quoted: bool
    ; parts: part list
    ; loc: Loc.t
    }

  val string_of_var : var -> string
end

(** The S-expression type *)
type t =
  | Atom of Atom.t
  | Quoted_string of string
  | List of t list
  | Template of Template.t

val atom : string -> t
(** [atom s] convert the string [s] to an Atom.
    @raise Invalid_argument if [s] does not satisfy [Atom.is_valid s]. *)

val atom_or_quoted_string : string -> t

val unsafe_atom_of_string : string -> t

(** Serialize a S-expression *)
val to_string : t -> syntax:syntax -> string

(** Serialize a S-expression using indentation to improve readability *)
val pp : syntax -> Format.formatter -> t -> unit

(** Serialization that never fails because it quotes atoms when necessary
    TODO remove this once we have a proper sexp type *)
val pp_quoted : Format.formatter -> t -> unit

(** Same as [pp ~syntax:Dune], but split long strings. The formatter
    must have been prepared with [prepare_formatter]. *)
val pp_split_strings : Format.formatter -> t -> unit

(** Prepare a formatter for [pp_split_strings]. Additionaly the
    formatter escape newlines when the tags "makefile-action" or
    "makefile-stuff" are active. *)
val prepare_formatter : Format.formatter -> unit

(** Abstract syntax tree *)
module Ast : sig
  type sexp = t
  type t =
    | Atom of Loc.t * Atom.t
    | Quoted_string of Loc.t * string
    | Template of Template.t
    | List of Loc.t * t list

  val atom_or_quoted_string : Loc.t -> string -> t

  val loc : t -> Loc.t

  val remove_locs : t -> sexp
end with type sexp := t

val add_loc : t -> loc:Loc.t -> Ast.t

module Parse_error : sig
  type t

  val loc     : t -> Loc.t
  val message : t -> string
end

(** Exception raised in case of a parsing error *)
exception Parse_error of Parse_error.t

module Lexer : sig
  module Token : sig
    type t =
      | Atom          of Atom.t
      | Quoted_string of string
      | Lparen
      | Rparen
      | Sexp_comment
      | Eof
      | Template of Template.t
  end

  type t = Lexing.lexbuf -> Token.t

  val token : t
  val jbuild_token : t
end

module Parser : sig
  module Mode : sig
    type 'a t =
      | Single      : Ast.t t
      | Many        : Ast.t list t
      | Many_as_one : Ast.t t
  end

  val parse
    :  mode:'a Mode.t
    -> ?lexer:Lexer.t
    -> Lexing.lexbuf
    -> 'a
end

val parse_string
  :  fname:string
  -> mode:'a Parser.Mode.t
  -> ?lexer:Lexer.t
  -> string
  -> 'a
