The @all alias should only build enabled libraries
  $ dune build @all

Note that nothing was built
  $ ls _build/default/disabled/
  dune
  foo.ml
