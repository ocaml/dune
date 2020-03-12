let syntax =
  Dune_lang.Syntax.createn ~name:"menhir" ~desc:"the menhir extension"
    [ ((1, 0), `Since (1, 0))
    ; ((1, 1), `Since (1, 4))
    ; ((2, 0), `Since (1, 4))
    ; ((2, 1), `Since (2, 2))
    ]
