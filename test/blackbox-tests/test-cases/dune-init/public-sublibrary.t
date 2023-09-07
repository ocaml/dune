Sub-library names should be accepted:

  $ dune init lib lib_s1_s2 --public lib.sub1.sub2
  dune: option '--public': invalid component name `lib.sub1.sub2'
        Library names must be non-empty and composed only of the
        following
        characters: 'A'..'Z', 'a'..'z', '_' or '0'..'9'.
  Usage: dune init library [OPTION]… NAME [PATH]
  Try 'dune init library --help' or 'dune --help' for more information.
  [1]
