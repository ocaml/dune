Testing the usage of the (dirs ...) stanza in the precense of a subdirectory

Building as usual should work.
  $ cat > dune << EOF
  > (dirs :standard)
  > EOF

  $ dune build A/a
  Error: Don't know how to build A/a
  [1]
  $ dune build B/b
  Error: Don't know how to build B/b
  [1]
  $ dune build A/C/c
  Error: Don't know how to build A/C/c
  [1]

If we exclude A/, dune should complain it does not know how to build it and it's
children.
  $ cat > dune << EOF
  > (dirs :standard \ A)
  > EOF

  $ dune build A/a
  $ dune build B/b
  Error: Don't know how to build B/b
  [1]
  $ dune build A/C/c

Same for the other directory.
  $ cat > dune << EOF
  > (dirs :standard \ B)
  > EOF

  $ dune build A/a
  Error: Don't know how to build A/a
  [1]
  $ dune build B/b
  $ dune build A/C/c
  Error: Don't know how to build A/C/c
  [1]

If we wish to exclude a subdirectory of A/, the following should error and Dune
should suggest the correct course of action.
  $ cat > dune << EOF
  > (dirs :standard \ A/C)
  > EOF

  $ dune build A/a
  File "dune", line 1, characters 18-21:
  1 | (dirs :standard \ A/C)
                        ^^^
  Error: only immediate sub-directories may be specified.
  Hint: to ignore A/C, write "(dirs C)" in A/dune or use the "(subdirs)"
  stanza.
  [1]

This should now only fail for the excluded directory.
  $ cat > dune << EOF
  > (subdir A (dirs :standard \ C))
  > EOF

  $ dune build A/a
  $ dune build B/b
  $ dune build A/C/c
