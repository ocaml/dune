Show that copy_files errors out if no files are found

  $ mkdir -p target foo
  $ make_dune_project 3.17
  $ cat >target/dune <<EOF
  > (copy_files
  >  (files ../foo/*.txt))
  > EOF

  $ dune build
  File "target/dune", line 2, characters 8-20:
  2 |  (files ../foo/*.txt))
              ^^^^^^^^^^^^
  Error: Does not match any files
  [1]

It doesn't error out in older dune lang versions

  $ make_dune_project 3.16

  $ dune build
