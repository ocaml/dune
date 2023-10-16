The use of (include_subdirs qualified) should be compatible with the use of
Menhir in other places than the root of the file hierarchy.

  $ dune exec ./foo.exe
  File "bar/dune", line 1, characters 0-23:
  1 | (menhir
  2 |  (modules baz))
  Error: I can't determine what library/executable the files produced by this
  stanza are part of.
  [1]
