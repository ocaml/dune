  $ dune build --root test-stanza
  Entering directory 'test-stanza'
  File "dune", line 1, characters 0-49:
  1 | (alias
  2 |  (name runtest)
  3 |  (action (echo "test\n")))
  Error: I can't determine automatically which package this stanza is for.
  I have the choice between these ones:
  - bar (because of bar.opam)
  - foo (because of foo.opam)
  You need to add a (package ...) field to this (alias) stanza.
  [1]
