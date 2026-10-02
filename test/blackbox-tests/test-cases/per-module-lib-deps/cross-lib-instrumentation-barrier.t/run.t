Per-module narrowing on an unwrapped library declaring
`(instrumentation (backend X))` without `--instrument-with` at
build time. The narrowing must (a) not demand non-existent
`.pp.ml` files from the instrumentation-disabled lib, and (b)
emit per-module deps instead of the cctx-wide `.cmi` glob over
`middle`'s objdir.

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

Build without `--instrument-with`. The narrowing produces per-
module narrow deps on the consumer's compile rule.

  $ dune build consumer/consumer.exe

The consumer's compile rule has no glob entry over `middle`'s
objdir.

  $ dune rules --root . --format=json --deps '%{cmo:consumer/consumer}' > deps.json

  $ jq -r 'include "dune"; .[] | depsGlobs
  >   | select(.dir | endswith("middle/.middle.objs/byte"))
  >   | .dir + " " + .predicate' < deps.json
