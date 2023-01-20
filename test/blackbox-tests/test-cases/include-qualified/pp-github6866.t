Preprocessing should work when there's modules with the same name

  $ cat >dune-project <<EOF
  > (lang dune 3.7)
  > EOF

  $ cat >dune <<EOF
  > (include_subdirs qualified)
  > (library
  >  (name foo)
  >  (preprocess (action (system "cat %{input-file}"))))
  > EOF

  $ mkdir bar/
  $ touch baz.ml bar/baz.ml

  $ dune build @check
