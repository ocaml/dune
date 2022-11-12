If two packages are available and no (package) is present, an error message is
displayed. This can happen for:

- (executable)

  $ dune build
  File "dune", line 1, characters 0-41:
  1 | (executable
  2 |  (public_name an_executable))
  Error: I can't determine automatically which package this stanza is for.
  I have the choice between these ones:
  - pkg1 (because of pkg1.opam)
  - pkg2 (because of pkg2.opam)
  You need to add a (package ...) field to this (executable) stanza.
  [1]
