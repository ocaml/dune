Reproducer for the bug introduced by storing post-pp [Module.t] in
[Lib_index]: a library declares [(instrumentation (backend X))]
without any [(preprocess ...)]. Instrumentation is *disabled* at
build time (no [--instrument-with] flag), so dune does not produce
[.pp.ml] files. But [Lib_info.preprocess] still returns
[Pps { pps = [Instrumentation_backend X]; ... }] (the static
config), and [build_lib_index] currently maps that to
[Module.pped (Module.ml_source m)] — i.e. paths like [foo.pp.ml] —
which dune then can't find a rule for when the cross-library
walker reads ocamldep on those modules.

  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > EOF

[middle] declares an instrumentation backend but no
[(preprocess ...)]. With instrumentation disabled at build time,
no [.pp.ml] files are produced.

  $ mkdir leaf
  $ cat > leaf/dune <<EOF
  > (library (name leaf) (wrapped false))
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

Build without [--instrument-with]: the cross-library walker
should NOT demand [middle.pp.ml] / [middle.pp.mli]. With the
buggy mapping that demands them anyway, dune reports
"No rule found for middle.pp.ml".

  $ dune build consumer/consumer.exe

The cross-library walker should produce per-module narrow deps
on the consumer's compile rule, not a wide glob over [middle]'s
objdir. Assert no glob entry over [middle]'s objdir.

  $ dune rules --root . --format=json --deps _build/default/consumer/.consumer.eobjs/byte/dune__exe__Consumer.cmo > deps.json

  $ jq -r 'include "dune"; .[] | depsGlobs
  >   | select(.dir | endswith("middle/.middle.objs/byte"))
  >   | .dir + " " + .predicate' < deps.json
