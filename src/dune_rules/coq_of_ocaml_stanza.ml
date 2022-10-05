open Import
open Dune_lang.Decoder

let syntax =
  Dune_lang.Syntax.create ~name:"coq-of-ocaml" ~desc:"coq-of-ocaml support"
    [ ((0, 1), `Since (3, 7)) ]

type t =
  { modules : string list
  ; loc : Loc.t
  }

let decode =
  fields
    (let+ modules = field "modules" (repeat string)
     and+ loc = loc in
     { modules; loc })

type Stanza.t += T of t

let () =
  Dune_project.Extension.register_simple syntax
    (return [ ("coq-of-ocaml", decode >>| fun x -> [ T x ]) ])
