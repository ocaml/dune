@all does not depend directly on file copies from the source tree

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > EOF

Add two files

  $ touch a.ml b.ml

An empty project, should not copy any file.

  $ dune build
  $ find _build/default -name '*.ml'

A project that only uses a.ml, should not copy b.ml

  $ cat > dune <<EOF
  > (library (name a) (modules a))
  > EOF
  $ dune build
  $ find _build/default -name '*.ml'
  _build/default/a.ml

A project that uses both files, should copy both.

  $ cat > dune <<EOF
  > (library (name a))
  > EOF
  $ dune build
  $ find _build/default -name '*.ml' | sort
  _build/default/a.ml
  _build/default/b.ml
