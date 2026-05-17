Test that `(melange.pps ...)` applies only to Melange compilation.

The field is available starting in Dune 3.24.

  $ mkdir old
  $ cat > old/dune-project <<EOF
  > (lang dune 3.23)
  > (using melange 0.1)
  > EOF
  $ cat > old/dune <<EOF
  > (library
  >  (name old)
  >  (modes melange)
  >  (melange.pps melange.ppx))
  > EOF
  $ dune build --root old
  Entering directory 'old'
  File "dune", line 4, characters 1-26:
  4 |  (melange.pps melange.ppx))
       ^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: 'melange.pps' is only available since version 3.24 of the dune
  language. Please update your dune-project file to have (lang dune 3.24).
  Leaving directory 'old'
  [1]

  $ mkdir app
  $ cat > app/dune-project <<EOF
  > (lang dune 3.24)
  > (using melange 0.1)
  > EOF
  $ cat > app/dune <<EOF
  > (library
  >  (name app)
  >  (modes melange :standard)
  >  (melange.pps noop_ppx))
  > EOF

  $ mkdir app/ppx
  $ cat > app/ppx/dune <<EOF
  > (library
  >  (name noop_ppx)
  >  (kind ppx_rewriter)
  >  (libraries ppxlib))
  > EOF
  $ cat > app/ppx/noop_ppx.ml <<EOF
  > let () = Ppxlib.Driver.register_transformation "noop_ppx"
  > EOF

  $ cat > app/foo.ml <<EOF
  > let x = "foo"
  > EOF
  $ cat > app/melange_only.melange.ml <<EOF
  > let x = "melange only"
  > EOF

  $ dune build --root app
  $ find ./app/_build/default -name '*.pp.ml' | censor | sort
  ./app/_build/default/.melange_src/foo.pp.ml
  ./app/_build/default/.melange_src/melange_only.pp.ml
