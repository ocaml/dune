include Sexp

module Io = struct
  let load ?lexer path ~mode =
    Io.with_lexbuf_from_file path ~f:(Usexp.Parser.parse ~mode ?lexer)
end

module type Sexpable = sig
  type t
  val t : t Of_sexp.t
  val sexp_of_t : t To_sexp.t
end
