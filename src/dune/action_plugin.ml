let syntax =
  Dune_lang.Syntax.createn ~name:"action-plugin" ~desc:"action plugin extension"
    [ ((0, 1), `Since (2, 0)) ]
