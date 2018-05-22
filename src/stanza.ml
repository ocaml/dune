open Stdune

type t = ..

module Parser = struct
  type nonrec t = t list Sexp.Of_sexp.Constructor_spec.t
end
