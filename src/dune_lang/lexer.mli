module Token = Lexer_shared.Token

type t = with_comments:bool -> Lexing.lexbuf -> Token.t

val token : t
val jbuild_token : t

val of_syntax : Syntax.t -> t

module Error : sig
  type t =
    { start   : Lexing.position
    ; stop    : Lexing.position
    ; message : string
    }
end

exception Error of Error.t
