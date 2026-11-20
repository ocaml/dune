Public aliases into installed private libraries should work in both modes.

  $ mkdir -p lib/public lib/private app
  $ cat > lib/dune-project <<EOF
  > (lang dune 3.25)
  > (using melange 1.0)
  > (package (name foo))
  > EOF
  $ cat > lib/private/dune <<EOF
  > (library
  >  (name priv)
  >  (package foo)
  >  (modes byte melange))
  > EOF
  $ echo 'let value = 1' > lib/private/priv.ml
  $ cat > lib/public/dune <<EOF
  > (library
  >  (public_name foo)
  >  (modes byte melange)
  >  (libraries priv))
  > EOF
  $ echo 'module P = Priv' > lib/public/foo.ml
  $ dune build @install --root lib
  $ dune install --prefix "$PWD/prefix" --root lib --display quiet
  $ export OCAMLPATH="$PWD/prefix/lib:$OCAMLPATH"

  $ cat > app/dune-project <<EOF
  > (lang dune 3.25)
  > (using melange 1.0)
  > EOF
  $ cat > app/dune <<EOF
  > (library
  >  (name entry)
  >  (modes byte melange)
  >  (libraries foo))
  > EOF
  $ cd app
  $ echo 'let value = Foo.P.value' > entry.ml

Both OCaml and Melange resolve the alias.

  $ dune build .entry.objs/byte/entry.cmo
  $ dune build .entry.objs/melange/entry.cmj

Direct references to the private library remain unavailable in both modes.

  $ echo 'let value = Priv.value' > entry.ml
  $ dune build .entry.objs/byte/entry.cmo
  File "entry.ml", line 1, characters 12-16:
  1 | let value = Priv.value
                  ^^^^
  Error: Unbound module Priv
  [1]
  $ dune build .entry.objs/melange/entry.cmj
  File "entry.ml", line 1, characters 12-16:
  1 | let value = Priv.value
                  ^^^^
  Error: Unbound module Priv
  [1]
