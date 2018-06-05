This behavior is surprising, we should get an error about the fact
that ${@} is not quoted and doesn't contain exactly 1 element

  $ dune build --root bad x
  Entering directory 'bad'
  Error: Rule failed to generate the following targets:
  - x
  - y
  [1]


The targets should only be interpreted as a single path when quoted

  $ dune build --root good s
  Entering directory 'good'
  Error: Rule failed to generate the following targets:
  - s
  - t
  [1]
