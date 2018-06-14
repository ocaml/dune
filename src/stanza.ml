open Stdune

type t = ..

module Parser = struct
  type nonrec t = string * t list Sexp.Of_sexp.cstr_parser
end
