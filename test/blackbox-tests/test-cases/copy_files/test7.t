Show that copy_files errors out if no files are found

  $ mkdir -p target foo
  $ cat >dune-project <<EOF
  > (lang dune 3.17)
  > EOF
  $ cat >target/dune <<EOF
  > (copy_files
  >  (files ../foo/*.txt))
  > EOF

  $ dune build
  File "target/dune", line 2, characters 8-20:
  2 |  (files ../foo/*.txt))
              ^^^^^^^^^^^^
  Error: Cannot find any files to copy
  [1]
