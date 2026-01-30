Preprocessing should work when there's modules with the same name

  $ make_dune_project 3.7

  $ cat >dune <<EOF
  > (include_subdirs qualified)
  > (library
  >  (name foo)
  >  (preprocess (action (system "cat %{input-file}"))))
  > EOF

  $ mkdir bar/
  $ touch baz.ml bar/baz.ml

  $ dune build @check
