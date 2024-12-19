Menhir parsers in qualified subdirectories should be able to refer to sibling modules:

  $ dune build
  File "lang/parser.mly", line 3, characters 8-16:
  Error: Unbound module Ast
  [1]
