Testing composition of theories accross a dune workspace with cyclic
dependencies.

  $ dune build A
  Error: Dependency cycle between:
     theory A in A
  -> theory B in B
  -> theory C in C
  -> theory A in A
  -> required by _build/default/A/a.v.d
  -> required by _build/default/A/.a.aux
  -> required by alias A/all
  -> required by alias A/default
  [1]

  $ dune build B
  Error: Dependency cycle between:
     theory B in B
  -> theory C in C
  -> theory A in A
  -> theory B in B
  -> required by _build/default/B/b.v.d
  -> required by _build/default/B/.b.aux
  -> required by alias B/all
  -> required by alias B/default
  [1]

  $ dune build C
  Error: Dependency cycle between:
     theory C in C
  -> theory A in A
  -> theory B in B
  -> theory C in C
  -> required by _build/default/C/c.v.d
  -> required by _build/default/C/.c.aux
  -> required by alias C/all
  -> required by alias C/default
  [1]
