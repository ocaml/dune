Generate the source of an executable in a subdir:

  $ cat >dune-project <<EOF
  > (lang dune 3.2)
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (with-stdout-to foo/bar.ml (echo "let foo = 42;;")))
  > (include_subdirs unqualified)
  > (executable (name bar))
  > EOF

  $ dune exec --display short ./bar.exe
        ocamlc .bar.eobjs/byte/dune__exe__Bar.{cmi,cmti}
  File "dune", line 4, characters 0-23:
  4 | (executable (name bar))
      ^^^^^^^^^^^^^^^^^^^^^^^
  Error: No rule found for bar.ml
  [1]
