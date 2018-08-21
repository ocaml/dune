include Sexp

module Io = struct
  let load ?lexer path ~mode =
    Io.with_lexbuf_from_file path ~f:(Usexp.Parser.parse ~mode ?lexer)
end
