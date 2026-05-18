An unwrapped library has two modules with mixed preprocessing: `a`
uses default preprocessing, `b` uses `(staged_pps ...)`. The
consumer references `A.identity` whose type is `B.t -> B.t`. Pins
that the consumer's compile correctly tracks `b.cmi` as a sandbox-
required dep — even though the consumer never names `B` in source.

  $ make_dune_project 3.24

A no-op staged ppx, modelled on
`test/blackbox-tests/test-cases/staged-pps-relative-directory-gh8158.t`.
The driver copies its input verbatim.

  $ mkdir ppx
  $ cat > ppx/dune <<EOF
  > (library
  >  (name ppx_noop)
  >  (kind ppx_rewriter)
  >  (ppx.driver (main Ppx_noop.main)))
  > EOF
  $ cat > ppx/ppx_noop.ml <<EOF
  > let main () =
  >   let n = Array.length Sys.argv in
  >   if n < 2 then assert false;
  >   let input = Sys.argv.(n - 2) in
  >   let output = Sys.argv.(n - 1) in
  >   Filename.quote_command "cp" [input; output]
  >   |> Sys.command
  >   |> exit
  > EOF

`mylib` is `(wrapped false)` with `a` default-pp and `b` staged-pps.
`a`'s interface mentions `B.t`, so the consumer's call to
`A.identity` forces the compiler to load `b.cmi` to resolve the
type.

  $ mkdir mylib
  $ cat > mylib/dune <<EOF
  > (library
  >  (name mylib)
  >  (wrapped false)
  >  (preprocess (per_module ((staged_pps ppx_noop) b))))
  > EOF
  $ cat > mylib/a.mli <<EOF
  > val identity : B.t -> B.t
  > EOF
  $ cat > mylib/a.ml <<EOF
  > let identity (x : B.t) = x
  > EOF
  $ cat > mylib/b.ml <<EOF
  > type t = int
  > let zero : t = 0
  > EOF

`consumer` references `A.identity` but never names `B`. The
`--sandbox=copy` build below is the discriminator: if a regression
dropped `b.cmi` from the consumer's compile-rule deps, the sandbox
would not stage it and the build would fail with "no such file"
deterministically rather than passing silently from a stale
`_build/`.

  $ mkdir consumer
  $ cat > consumer/dune <<EOF
  > (executable (name consumer) (libraries mylib))
  > EOF
  $ cat > consumer/consumer.ml <<EOF
  > let _ = A.identity 0
  > EOF

  $ dune build --sandbox=copy consumer/consumer.exe
