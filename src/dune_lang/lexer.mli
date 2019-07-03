module Token = Lexer_shared.Token

type t = with_comments:bool -> Lexing.lexbuf -> Token.t

val token : t
val jbuild_token : t

val of_syntax : File_syntax.t -> t
