Interaction of the foreign_library stanza and the enabled_if field.

  $ make_dune_project 3.18

  $ cat > a.c <<EOF
  > int foo() {
  >   return 1;
  > }
  > EOF

  $ write_enabled_if_foreign_libraries() {
  > : > dune
  > for enabled_if in "$@"; do
  > cat >> dune <<EOF
  > (foreign_library
  >  (enabled_if ${enabled_if})
  >  (language c)
  >  (archive_name a)
  >  (names a))
  > 
  > EOF
  > done
  > }

We should allow multiple foreign libraries to define the same archive if only
one of them is enabled:

  $ write_enabled_if_foreign_libraries true false false
  $ dune build


Repeat the test, but now two of the libraries are indeed enabled which is
illegal:

  $ write_enabled_if_foreign_libraries true true false

  $ dune build
  File "dune", line 5, characters 8-9:
  5 |  (names a))
              ^
  Error: Multiple definitions for the same object file "a". See another
  definition at dune:11.
  Hint: You can avoid the name clash by renaming one of the objects, or by
  placing it into a different directory.
  [1]
