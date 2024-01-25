Show that copy_files operates on the build folder

  $ mkdir -p target foo
  $ cat >dune-project <<EOF
  > (lang dune 3.14)
  > EOF
  $ cat >target/dune <<EOF
  > (copy_files
  >  (files ../foo/*.txt))
  > EOF
  $ cat >foo/dune <<EOF
  > (rule
  >  (with-stdout-to in-build.txt
  >  (echo "")))
  > EOF

  $ touch foo/in-source.txt

  $ dune build target/in-source.txt
  $ dune build target/in-build.txt

Show the difference when `only_sources` is used

  $ cat >target/dune <<EOF
  > (copy_files
  >  (only_sources true)
  >  (files ../foo/*.txt))
  > EOF

  $ dune build target/in-source.txt
  $ dune build target/in-build.txt
  Error: Don't know how to build target/in-build.txt
  [1]
