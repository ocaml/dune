This behavior is surprising, we should get an error about the fact
that ${@} is not quoted and doesn't contain exactly 1 element

  $ dune build x
  Error: Rule failed to generate the following targets:
  - x
  - y
  [1]

