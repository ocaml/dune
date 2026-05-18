Regression guard for cross-library transitive [.cmi] reads through
an intermediate library whose stanza injects [-open] on the leaf.
The intermediate's interface mentions a type from a leaf library
that the consumer never names syntactically; the intermediate's
own source also never names the leaf, because [-open] hides it.

[prelude] defines [color = Red | Green | Blue]. [middle] depends
on [prelude] and is compiled with [(flags (:standard -open
Prelude))], exposing [val pick : unit -> color] (where [color]
resolves through the open to [Prelude.color]). [consumer] depends
on both libraries and pattern-matches on the result of
[Middle.pick] against the bare constructors [Green], [Red], [Blue].

The consumer's compile genuinely needs [prelude.cmi] so the OCaml
compiler can resolve the constructors back to [Prelude.color]. The
per-module BFS over [ocamldep -modules] cannot find this dep:
ocamldep on [middle.{ml,mli}] reports no token [Prelude] (the
source uses the open), and ocamldep on [consumer.ml] reports only
[Middle]. None of the three [must_glob_libs] recovery branches
catch [prelude]: it is not wrapped, not a ppx-runtime lib, not a
virtual-impl.

  $ make_dune_project 3.23

  $ mkdir prelude middle consumer

  $ cat > prelude/dune <<EOF
  > (library (name prelude) (wrapped false))
  > EOF
  $ cat > prelude/prelude.ml <<EOF
  > type color = Red | Green | Blue
  > EOF

  $ cat > middle/dune <<EOF
  > (library
  >  (name middle)
  >  (wrapped false)
  >  (libraries prelude)
  >  (flags (:standard -open Prelude)))
  > EOF
  $ cat > middle/middle.mli <<EOF
  > val pick : unit -> color
  > EOF
  $ cat > middle/middle.ml <<EOF
  > let pick () = Green
  > EOF

  $ cat > consumer/dune <<EOF
  > (executable (name consumer) (libraries middle prelude))
  > EOF
  $ cat > consumer/consumer.ml <<EOF
  > let () = match Middle.pick () with
  >   | Green -> print_endline "g"
  >   | Red | Blue -> print_endline "nb"
  > EOF

Sandbox-forced build of the consumer succeeds: [prelude]'s [.cmi]
is declared as a compile-rule dep through the per-module filter's
cross-lib walk and is therefore present in the sandbox.

  $ dune build --sandbox=copy consumer/consumer.exe
