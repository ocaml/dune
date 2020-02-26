open Stdune

module Token : sig
  module Comment : sig
    type t =
      | Lines of string list
          (** The following comment:

              {v ; abc ; def v}

              is represented as:

              {[ Lines [ " abc"; " def" ] ]} *)
      | Legacy
          (** Legacy for jbuild files: either block comments or sexp comments.
              The programmer is responsible for fetching the comment contents
              using the location. *)

    val to_dyn : t -> Dyn.t
  end

  type t =
    | Atom of Atom.t
    | Quoted_string of string
    | Lparen
    | Rparen
    | Sexp_comment
    | Eof
    | Template of Template.t
    | Comment of Comment.t
end

type t = with_comments:bool -> Lexing.lexbuf -> Token.t

val token : t
