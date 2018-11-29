  $ jbuilder build --root pre-1.6 data/dune
  Entering directory 'pre-1.6'
  Don't know how to build data/dune
  [1]
  $ jbuilder build --root pre-1.6 old-style/data/dune
  Entering directory 'pre-1.6'
  $ dune build --root 1.6 @runtest
  Entering directory '1.6'
  real dir
  $ dune build --root glob @runtest
  Entering directory 'glob'
  real dir
  $ dune build --root logical @runtest
  Entering directory 'logical'
