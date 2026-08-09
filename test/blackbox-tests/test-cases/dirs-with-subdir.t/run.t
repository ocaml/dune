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

In older dune language versions, nested paths in (dirs ...) are ignored.
  $ cat > dune << EOF
  > (dirs :standard \ A/C)
  > EOF

  $ dune build A/a
  $ dune build A/C/c

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

Starting from dune 3.25, a nested path in (dirs ...) is rejected and Dune should
suggest the correct course of action.
  $ mkdir -p v325/A/C
  $ touch v325/A/a v325/A/C/c
  $ cat > v325/dune-project << EOF
  > (lang dune 3.25)
  > EOF
  $ cat > v325/dune << EOF
  > (dirs :standard \ A/C)
  > EOF

  $ (cd v325 && dune build A/a)
  File "dune", line 1, characters 18-21:
  1 | (dirs :standard \ A/C)
                        ^^^
  Error: only immediate sub-directories may be specified.
  Hint: to ignore A/C, write "(dirs C)" in A/dune
  [1]
