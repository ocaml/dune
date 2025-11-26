Testing composition of theories across a dune workspace with cyclic
dependencies.

  $ dune build A
  Error: Dependency cycle between:
     theory A in A/dune:2
  -> theory B in B/dune:2
  -> theory C in C/dune:2
  -> theory A in A/dune:2
  -> required by _build/default/A/.A.theory.d
  -> required by alias A/all
  -> required by alias A/default
  [1]

  $ dune build B
  Error: Dependency cycle between:
     theory B in B/dune:2
  -> theory C in C/dune:2
  -> theory A in A/dune:2
  -> theory B in B/dune:2
  -> required by _build/default/B/.B.theory.d
  -> required by alias B/all
  -> required by alias B/default
  [1]

  $ dune build C
  Error: Dependency cycle between:
     theory C in C/dune:2
  -> theory A in A/dune:2
  -> theory B in B/dune:2
  -> theory C in C/dune:2
  -> required by _build/default/C/.C.theory.d
  -> required by alias C/all
  -> required by alias C/default
  [1]
