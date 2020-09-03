open! Dune_engine

let syntax =
  Dune_lang.Syntax.create ~name:"menhir" ~desc:"the menhir extension"
    [ ((1, 0), `Since (1, 0))
    ; ((1, 1), `Since (1, 4))
    ; ((2, 0), `Since (1, 4))
    ; ((2, 1), `Since (2, 2))
    ]
