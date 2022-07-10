Testing composition of theories accross a dune workspace with cyclic
dependencies.

  $ dune build A
  Error: Dependency cycle between:
     theory A in A
  -> theory B in B
  -> theory C in C
  -> theory A in A
  [1]

  $ dune build B
  Error: Dependency cycle between:
     theory B in B
  -> theory C in C
  -> theory A in A
  -> theory B in B
  [1]

  $ dune build C
  Error: Dependency cycle between:
     theory C in C
  -> theory A in A
  -> theory B in B
  -> theory C in C
  [1]
