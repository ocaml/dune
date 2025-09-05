Interaction of the foreign_library stanza and the enabled_if field.

  $ cat > dune-project <<EOF
  > (lang dune 3.18)
  > EOF

  $ cat > a.c <<EOF
  > int foo() {
  >   return 1;
  > }
  > EOF

We should allow multiple foreign libraries to define the same archive if only
one of them is enabled:

  $ cat > dune <<EOF
  > (foreign_library
  >  (enabled_if true)
  >  (language c)
  >  (archive_name a)
  >  (names a))
  > 
  > (foreign_library
  >  (enabled_if false)
  >  (language c)
  >  (archive_name a)
  >  (names a))
  > 
  > (foreign_library
  >  (enabled_if false)
  >  (language c)
  >  (archive_name a)
  >  (names a))
  > EOF
  $ dune build


Repeat the test, but now two of the libraries are indeed enabled which is
illegal:

  $ cat > dune <<EOF
  > (foreign_library
  >  (enabled_if true)
  >  (language c)
  >  (archive_name a)
  >  (names a))
  > 
  > (foreign_library
  >  (enabled_if true)
  >  (language c)
  >  (archive_name a)
  >  (names a))
  > 
  > (foreign_library
  >  (enabled_if false)
  >  (language c)
  >  (archive_name a)
  >  (names a))
  > EOF

  $ dune build
  File "dune", line 5, characters 8-9:
  5 |  (names a))
              ^
  Error: Multiple definitions for the same object file "a". See another
  definition at dune:11.
  Hint: You can avoid the name clash by renaming one of the objects, or by
  placing it into a different directory.
  [1]
