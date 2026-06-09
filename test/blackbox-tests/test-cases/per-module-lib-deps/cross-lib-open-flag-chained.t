Like `cross-lib-open-flag-barrier.t`, but the intermediate library uses
two chained opens, `(flags (:standard -open Prelude -open Color))`. The
second open resolves through the first: `Color` is the `Color` submodule
of `prelude`, brought into scope by `-open Prelude`, rather than a
top-level module. Pins that the consumer's compile tracks `prelude`'s
`.cmi`s as sandbox-required deps.

  $ make_dune_project 3.24

`prelude` is a wrapped library whose `Color` submodule exposes a sum
type:

  $ mkdir prelude
  $ cat > prelude/dune <<EOF
  > (library (name prelude))
  > EOF
  $ cat > prelude/color.ml <<EOF
  > type t = Red | Green | Blue
  > EOF

`middle` depends on `prelude` and is compiled with
`(flags (:standard -open Prelude -open Color))`, exposing
`val pick : unit -> t` whose `t` resolves through the chained opens to
`Prelude.Color.t`. Its source names neither `Prelude` nor `Color`:

  $ mkdir middle
  $ cat > middle/dune <<EOF
  > (library
  >  (name middle)
  >  (libraries prelude)
  >  (flags (:standard -open Prelude -open Color)))
  > EOF
  $ cat > middle/middle.mli <<EOF
  > val pick : unit -> t
  > EOF
  $ cat > middle/middle.ml <<EOF
  > let pick () = Green
  > EOF

`consumer` depends on `middle` and `prelude` and pattern-matches on the
result of `Middle.pick` against the bare constructors `Green`, `Red`,
`Blue`. `ocamldep` reports no `Prelude` token on any of `middle.{ml,mli}`
or `consumer.ml`:

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
