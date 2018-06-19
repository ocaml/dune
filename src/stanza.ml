open Stdune

type t = ..

module Parser = struct
  type nonrec t = string * t list Sexp.Of_sexp.t
end
