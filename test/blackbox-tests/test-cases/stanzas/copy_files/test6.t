Show that copy_files operates on the build folder

  $ mkdir -p target foo
  $ make_dune_project 3.14
  $ cat >target/dune <<EOF
  > (copy_files
  >  (files ../foo/*.txt))
  > EOF
  $ cat >foo/dune <<EOF
  > (rule
  >  (write-file in-build.txt ""))
  > EOF

  $ touch foo/in-source.txt

  $ dune build target/in-source.txt
  $ dune build target/in-build.txt

Show the difference when `only_sources` is used

  $ cat >target/dune <<EOF
  > (copy_files
  >  (only_sources)
  >  (files ../foo/*.txt))
  > EOF

  $ dune build target/in-source.txt
  $ dune build target/in-build.txt
  Error: Don't know how to build target/in-build.txt
  [1]

A blang expression can be used:

  $ cat >target/dune <<EOF
  > (copy_files
  >  (only_sources (= x x))
  >  (files ../foo/*.txt))
  > EOF

  $ dune build target/in-source.txt
  $ dune build target/in-build.txt
  Error: Don't know how to build target/in-build.txt
  [1]

A file with a promotion rule does not match until it exists in the source tree.
The copy_files stanza does not trigger promotion of a missing source file.

  $ cat >target/dune <<EOF
  > (copy_files
  >  (only_sources true)
  >  (files ../foo/*.txt))
  > EOF
  $ cat >>foo/dune <<EOF
  > (rule
  >  (mode promote)
  >  (action (write-file promoted.txt "promoted\n")))
  > EOF

  $ dune build target/promoted.txt
  Error: Don't know how to build target/promoted.txt
  [1]
  $ test ! -e foo/promoted.txt

Once promoted, the file matches even though it is also a generated target.

  $ dune build foo/promoted.txt
  $ cat foo/promoted.txt
  promoted
  $ dune build target/promoted.txt
  $ cat _build/default/target/promoted.txt
  promoted

The unpromoted generated file still does not match.

  $ dune build target/in-build.txt
  Error: Don't know how to build target/in-build.txt
  [1]
