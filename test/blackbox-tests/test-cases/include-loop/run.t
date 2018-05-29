  $ dune build --display short
  File "dune", line 2, characters 0-15:
  Error: Recursive inclusion of jbuild files detected:
  File a.inc is included from c.inc:2
  --> included from b.inc:2
  --> included from a.inc:2
  --> included from dune:2
  [1]
