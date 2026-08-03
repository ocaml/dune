A single-module local library with no `(libraries ...)` but with
`(preprocess (pps X))` has non-empty *resolved* requires (X's
runtime libs added via `add_pp_runtime_deps`). Pins that the
consumer's compile correctly tracks the ppx runtime lib's `.cmi`
as a sandbox-required dep, even though the consumer never names
the runtime lib syntactically.

  $ make_dune_project 3.24

`hello` is the ppx runtime lib (single-module unwrapped, no library
deps of its own). It exposes `Hello.t`:

  $ mkdir hello
  $ cat > hello/dune <<EOF
  > (library (name hello))
  > EOF
  $ cat > hello/hello.ml <<EOF
  > type t = int
  > let zero : t = 0
  > EOF

`hello_ppx` is a no-op ppx_rewriter declaring `hello` as its
`ppx_runtime_libraries`:

  $ mkdir hello_ppx
  $ cat > hello_ppx/dune <<EOF
  > (library
  >  (name hello_ppx)
  >  (kind ppx_rewriter)
  >  (ppx_runtime_libraries hello)
  >  (libraries ppxlib))
  > EOF
  $ cat > hello_ppx/hello_ppx.ml <<EOF
  > let () =
  >   Ppxlib.Driver.register_transformation_using_ocaml_current_ast
  >     ~impl:(fun s -> s) "noop"
  > EOF

`middle` is single-module, has no `(libraries ...)`, and uses
`(preprocess (pps hello_ppx))`. Its interface mentions `Hello.t`:

  $ mkdir middle
  $ cat > middle/dune <<EOF
  > (library
  >  (name middle)
  >  (preprocess (pps hello_ppx)))
  > EOF
  $ cat > middle/middle.mli <<EOF
  > val helper : Hello.t -> Hello.t
  > EOF
  $ cat > middle/middle.ml <<EOF
  > let helper x = x
  > EOF

`consumer` depends on `middle`; references `Middle.helper` but
never names `Hello` in source.

  $ mkdir consumer
  $ cat > consumer/dune <<EOF
  > (executable (name consumer) (libraries middle))
  > EOF
  $ cat > consumer/consumer.ml <<EOF
  > let _ = Middle.helper 0
  > EOF

  $ dune build --sandbox=copy consumer/consumer.exe
