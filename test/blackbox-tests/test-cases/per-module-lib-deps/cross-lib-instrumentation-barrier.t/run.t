Regression baseline for an unwrapped library declaring
`(instrumentation (backend X))` without `--instrument-with` at
build time. Today, the consumer's compile rule depends on the
lib's full `.cmi` glob over its objdir.

  $ make_dune_project 3.24

`middle` declares an instrumentation backend but no
`(preprocess ...)`. With instrumentation disabled at build time,
no `.pp.ml` files are produced.

  $ mkdir leaf
  $ cat > leaf/dune <<EOF
  > (library (name leaf))
  > EOF
  $ cat > leaf/leaf.ml <<EOF
  > type t = int
  > let zero : t = 0
  > EOF

  $ mkdir middle
  $ cat > middle/dune <<EOF
  > (library
  >  (name middle)
  >  (wrapped false)
  >  (libraries leaf)
  >  (instrumentation (backend hello)))
  > EOF
  $ cat > middle/middle.mli <<EOF
  > val identity : Leaf.t -> Leaf.t
  > EOF
  $ cat > middle/middle.ml <<EOF
  > let identity x = x
  > EOF

  $ mkdir consumer
  $ cat > consumer/dune <<EOF
  > (executable (name consumer) (libraries middle))
  > EOF
  $ cat > consumer/consumer.ml <<EOF
  > let _ = Middle.identity 0
  > EOF

Build without `--instrument-with`. Today this succeeds with the
cctx-wide `.cmi` glob covering `leaf.cmi` through `middle`'s objdir
chain.

  $ dune build consumer/consumer.exe

The consumer's compile rule today carries a wide `.cmi` glob over
`middle`'s objdir.

  $ dune rules --root . --format=json --deps '%{cmo:consumer/consumer}' > deps.json

  $ jq_dune -r '.[] | depsGlobs
  >   | select(.dir | endswith("middle/.middle.objs/byte"))
  >   | .dir + " " + .predicate' < deps.json
  _build/default/middle/.middle.objs/byte *.cmi
