This behavior is surprising, we should get an error about the fact
that ${@} is not quoted and doesn't contain exactly 1 element

  $ dune build --root bad x 2>&1 | grep -v Entering
  Error: Rule failed to generate the following targets:
  - x
  - y


The targets should only be interpreted as a single path when quoted

  $ dune build --root good s 2>&1 | grep -v Entering
  Error: Rule failed to generate the following targets:
  - s
  - t
