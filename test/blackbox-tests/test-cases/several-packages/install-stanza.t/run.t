If two packages are available and no (package) is present, an error message is
displayed. This can happen for:

- (install)

  $ dune build
  File "dune", line 1, characters 0-43:
  1 | (install
  2 |  (section etc)
  3 |  (files file.conf))
  Error: I can't determine automatically which package this stanza is for.
  I have the choice between these ones:
  - pkg1 (because of pkg1.opam)
  - pkg2 (because of pkg2.opam)
  You need to add a (package ...) field to this (install) stanza.
  [1]
