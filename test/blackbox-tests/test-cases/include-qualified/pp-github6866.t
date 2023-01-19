Preprocessing should work when there's modules with the same name

  $ cat >dune-project <<EOF
  > (lang dune 3.7)
  > EOF

  $ cat >dune <<EOF
  > (include_subdirs qualified)
  > (library
  >  (name foo)
  >  (preprocess (action (cat system "cat %{input-file}"))))
  > EOF

  $ mkdir bar/
  $ touch baz.ml bar/baz.ml

  $ dune build @check
  Error: Multiple rules generated for _build/default/.foo.objs/baz.pp.ml.d:
  - <internal location>
  - <internal location>
  -> required by alias check
  [1]
