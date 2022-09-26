----------------------------------------------------------------------------------
Testsuite for https://github.com/ocaml/dune/issues/3046
`dune init` should raise proper errors when syntactically invalid arguments
are given as parameters
----------------------------------------------------------------------------------

`dune init exe main --libs="str gsl"` returns an informative parsing error

  $ dune init exe main --libs="str gsl"
  dune: option '--libs': invalid element in list ('str gsl'): expected a valid
        dune atom
  Usage: dune init [OPTION]… COMPONENT NAME [PATH]
  Try 'dune init --help' or 'dune --help' for more information.
  [1]

`dune init lib foo --ppx="foo bar"` returns an informative parsing error

  $ dune init lib foo --ppx="foo bar"
  dune: option '--ppx': invalid element in list ('foo bar'): expected a valid
        dune atom
  Usage: dune init [OPTION]… COMPONENT NAME [PATH]
  Try 'dune init --help' or 'dune --help' for more information.
  [1]

`dune init lib foo --public="some/invalid&name!"` returns an informative parsing error

  $ dune init lib foo --public="some/invalid&name!"
  dune: option '--public': invalid component name `some/invalid&name!'
        Library names must be non-empty and composed only of the
        following
        characters: 'A'..'Z', 'a'..'z', '_' or '0'..'9'.
  Usage: dune init [OPTION]… COMPONENT NAME [PATH]
  Try 'dune init --help' or 'dune --help' for more information.
  [1]
