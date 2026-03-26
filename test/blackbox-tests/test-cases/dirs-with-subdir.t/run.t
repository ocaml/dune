Testing the usage of the (dirs ...) stanza in the presence of a sub-directory

Building as usual should work.
  $ cat > dune << EOF
  > (dirs :standard)
  > EOF

  $ dune build A/a
  $ dune build B/b
  $ dune build A/C/c

If we exclude A/, dune should complain it does not know how to build it and it's
children.
  $ cat > dune << EOF
  > (dirs :standard \ A)
  > EOF

  $ dune build A/a
  Error: Don't know how to build A/a
  Hint: directory A exists on disk but is excluded by a (dirs ...) stanza at
  dune:1
  [1]
  $ dune build B/b
  $ dune build A/C/c
  Error: Don't know how to build A/C/c
  Hint: directory A exists on disk but is excluded by a (dirs ...) stanza at
  dune:1
  [1]

Same for the other directory.
  $ cat > dune << EOF
  > ; Pad to test location
  > (dirs :standard \ B)
  > EOF

  $ dune build A/a
  $ dune build B/b
  Error: Don't know how to build B/b
  Hint: directory B exists on disk but is excluded by a (dirs ...) stanza at
  dune:2
  [1]
  $ dune build A/C/c

If we wish to exclude a subdirectory of A/, the following should error and Dune
should suggest the correct course of action.
  $ cat > dune << EOF
  > (dirs :standard \ A/C)
  > EOF

  $ dune build A/a

This should now only fail for the excluded directory.
  $ cat > dune << EOF
  > (subdir A (dirs :standard \ C))
  > EOF

  $ dune build A/a
  $ dune build B/b
  $ dune build A/C/c
  Error: Don't know how to build A/C/c
  Hint: directory A/C exists on disk but is excluded by a (dirs ...) stanza at
  dune:1
  [1]
