open Stdune

type t = ..

module Parser = struct
  type nonrec t = string * t list Sexp.Of_sexp.t
end

let syntax =
  Syntax.create ~name:"dune" ~desc:"the dune language"
    [ (0, 0) (* Jbuild syntax *)
    ; (1, 0)
    ]
