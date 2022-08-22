If two packages are available and no (package) is present, an error message is
displayed. This can happen for:

- (executable)

  $ dune build --root executable
  Entering directory 'executable'
  File "dune", line 1, characters 0-41:
  1 | (executable
  2 |  (public_name an_executable))
  Error: I can't determine automatically which package this stanza is for.
  I have the choice between these ones:
  - pkg1 (because of pkg1.opam)
  - pkg2 (because of pkg2.opam)
  You need to add a (package ...) field to this (executable) stanza.
  [1]

- (documentation)

  $ dune build --root documentation
  Entering directory 'documentation'
  File "dune", line 1, characters 0-15:
  1 | (documentation)
      ^^^^^^^^^^^^^^^
  Error: I can't determine automatically which package this stanza is for.
  I have the choice between these ones:
  - pkg1 (because of pkg1.opam)
  - pkg2 (because of pkg2.opam)
  You need to add a (package ...) field to this (documentation) stanza.
  [1]

- (install)

  $ dune build --root install
  Entering directory 'install'
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
