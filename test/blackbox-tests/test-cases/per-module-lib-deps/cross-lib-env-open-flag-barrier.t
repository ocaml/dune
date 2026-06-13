A consumer references a constructor of a leaf library's type
through an intermediate library where `-open Prelude` is injected
by an `(env ...)` stanza, not by the intermediate library's own
`(flags ...)` field. The intermediate's stanza uses `:standard`.
Pins that env-stanza-injected `-open` modules extend the BFS
frontier of [cross_lib_tight_set] identically to stanza-flag
`-open` modules — the dep lib's `:standard` spec must not
short-circuit out the env-injected edges.

  $ make_dune_project 3.24

`prelude` exposes a sum type:

  $ mkdir prelude
  $ cat > prelude/dune <<EOF
  > (library (name prelude) (wrapped false))
  > EOF
  $ cat > prelude/prelude.ml <<EOF
  > type color = Red | Green | Blue
  > EOF

`middle` is `(wrapped false)` so the per-module narrowing walks
its modules; depends on `prelude`; uses `:standard` flags. The
adjacent `(env ...)` stanza injects `-open Prelude` into the
default profile, so middle's modules see `Prelude.color`'s
constructors without naming `Prelude`:

  $ mkdir middle
  $ cat > middle/dune <<EOF
  > (env (_ (flags (:standard -open Prelude))))
  > (library
  >  (name middle)
  >  (wrapped false)
  >  (libraries prelude))
  > EOF
  $ cat > middle/m.mli <<EOF
  > val pick : unit -> color
  > EOF
  $ cat > middle/m.ml <<EOF
  > let pick () = Green
  > EOF

`consumer` depends on `middle` and `prelude` and pattern-matches
on the result of `M.pick` against the bare constructors `Green`,
`Red`, `Blue`. ocamldep on `m.{ml,mli}` and `consumer.ml` reports
no `Prelude` token; the type-checker needs `prelude.cmi` to
resolve the constructors.

  $ mkdir consumer
  $ cat > consumer/dune <<EOF
  > (executable (name consumer) (libraries middle prelude))
  > EOF
  $ cat > consumer/consumer.ml <<EOF
  > let () = match M.pick () with
  >   | Green -> print_endline "g"
  >   | Red | Blue -> print_endline "nb"
  > EOF

  $ dune build --sandbox=copy consumer/consumer.exe
