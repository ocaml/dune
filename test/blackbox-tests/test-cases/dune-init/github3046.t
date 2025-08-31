----------------------------------------------------------------------------------
Testsuite for https://github.com/ocaml/dune/issues/3046
`dune init` should raise proper errors when syntactically invalid arguments
are given as parameters
----------------------------------------------------------------------------------

`dune init exe main --libs="str gsl"` returns an informative parsing error

  $ dune init exe main --libs="str gsl"
  dune: option '--libs': invalid element in list ('str gsl'): expected a valid
        dune atom
  Usage: dune init executable [OPTION]… NAME [PATH]
  Try 'dune init executable --help' or 'dune --help' for more information.
  [1]

`dune init lib foo --ppx="foo bar"` returns an informative parsing error

  $ dune init lib foo --ppx="foo bar"
  dune: option '--ppx': invalid element in list ('foo bar'): expected a valid
        dune atom
  Usage: dune init library [OPTION]… NAME [PATH]
  Try 'dune init library --help' or 'dune --help' for more information.
  [1]

`dune init lib foo --public="some/invalid&name!"` returns an informative parsing error

  $ dune init lib foo --public="some/invalid&name!"
  dune: option '--public': Public names are composed of an opam package name
        and optional
        dot-separated string suffixes.
        Package names can contain letters, numbers, '-', '_' and '+', and need
        to
        contain at least a letter.
  Usage: dune init library [OPTION]… NAME [PATH]
  Try 'dune init library --help' or 'dune --help' for more information.
  [1]
