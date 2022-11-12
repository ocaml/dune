If two packages are available and no (package) is present, an error message is
displayed. This can happen for:

- (documentation)

  $ dune build
  File "dune", line 1, characters 0-15:
  1 | (documentation)
      ^^^^^^^^^^^^^^^
  Error: I can't determine automatically which package this stanza is for.
  I have the choice between these ones:
  - pkg1 (because of pkg1.opam)
  - pkg2 (because of pkg2.opam)
  You need to add a (package ...) field to this (documentation) stanza.
  [1]
