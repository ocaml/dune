We try to build the documentation but there will be a clash between the
directory targets. Notice how the tex one fails before html one.
  $ dune build @check
  Error: the following both define the directory target:
  _build/default/base.tex
  - dune:9
  - dune:17
  [1]
