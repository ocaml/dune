A consumer references a constructor of a leaf library's type
through an intermediate library compiled with
`(flags (:standard -open Prelude))`. The intermediate's source
never names `Prelude` (the open hides it), and the consumer never
names `Prelude` either. Pins that the consumer's compile correctly
tracks `prelude`'s `.cmi` as a sandbox-required dep.

  $ make_dune_project 3.24

`prelude` exposes a sum type:

  $ mkdir prelude
  $ cat > prelude/dune <<EOF
  > (library (name prelude))
  > EOF
  $ cat > prelude/prelude.ml <<EOF
  > type color = Red | Green | Blue
  > EOF

`middle` depends on `prelude` and is compiled with
`(flags (:standard -open Prelude))`, exposing
`val pick : unit -> color` whose `color` resolves through the open
to `Prelude.color`:

  $ mkdir middle
  $ cat > middle/dune <<EOF
  > (library
  >  (name middle)
  >  (libraries prelude)
  >  (flags (:standard -open Prelude)))
  > EOF
  $ cat > middle/middle.mli <<EOF
  > val pick : unit -> color
  > EOF
  $ cat > middle/middle.ml <<EOF
  > let pick () = Green
  > EOF

`consumer` depends on `middle` and `prelude` and pattern-matches on
the result of `Middle.pick` against the bare constructors `Green`,
`Red`, `Blue`. `ocamldep` on `middle.{ml,mli}` and `consumer.ml`
reports no `Prelude` token in either case.

  $ mkdir consumer
  $ cat > consumer/dune <<EOF
  > (executable (name consumer) (libraries middle prelude))
  > EOF
  $ cat > consumer/consumer.ml <<EOF
  > let () = match Middle.pick () with
  >   | Green -> print_endline "g"
  >   | Red | Blue -> print_endline "nb"
  > EOF

  $ dune build --sandbox=copy consumer/consumer.exe
