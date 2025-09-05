  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > EOF

  $ cat > dune <<EOF
  > (executable (name foo))
  > EOF

  $ cat > foo.ml <<EOF
  > open Bar
  > open Baz
  > EOF

  $ cat > bar.ml <<EOF
  > open Baz
  > EOF

  $ cat > baz.ml <<EOF
  > open Bar
  > EOF

  $ dune build
  Error: Dependency cycle between:
     _build/default/.foo.eobjs/dune__exe__Baz.impl.all-deps
  -> _build/default/.foo.eobjs/dune__exe__Bar.impl.all-deps
  -> _build/default/.foo.eobjs/dune__exe__Baz.impl.all-deps
  -> required by _build/default/.foo.eobjs/dune__exe__Foo.impl.all-deps
  -> required by _build/default/foo.exe
  -> required by alias all
  -> required by alias default
  [1]


