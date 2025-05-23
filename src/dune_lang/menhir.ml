open Import

let syntax =
  Syntax.create
    ~name:"menhir"
    ~desc:"the menhir extension"
    [ (1, 0), `Since (1, 0)
    ; (1, 1), `Since (1, 4)
    ; (2, 0), `Since (1, 4)
    ; (2, 1), `Since (2, 2)
    ; (3, 0), `Since (3, 13)
    ]
;;

let explain_since = 3, 0
