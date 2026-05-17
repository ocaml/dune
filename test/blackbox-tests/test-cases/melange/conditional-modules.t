Show that `(melange.modules ...)` is gated on Dune 3.24 and applies only to
Melange compilation.

  $ mkdir old
  $ cat > old/dune-project <<EOF
  > (lang dune 3.23)
  > (using melange 0.1)
  > EOF
  $ cat > old/dune <<EOF
  > (library
  >  (name old)
  >  (modes melange)
  >  (melange.modules foo))
  > EOF
  $ dune build --root old
  Entering directory 'old'
  File "dune", line 4, characters 1-22:
  4 |  (melange.modules foo))
       ^^^^^^^^^^^^^^^^^^^^^
  Error: 'melange.modules' is only available since version 3.24 of the dune
  language. Please update your dune-project file to have (lang dune 3.24).
  Leaving directory 'old'
  [1]
  $ rm -rf old

  $ cat > dune-project <<EOF
  > (lang dune 3.24)
  > (using melange 0.1)
  > EOF
  $ mkdir app
  $ cat > app/dune <<EOF
  > (library
  >  (name app)
  >  (modes melange byte)
  >  (modules ocaml_only)
  >  (melange.modules melange_only))
  > EOF
  $ cat > app/ocaml_only.ml <<EOF
  > let x = "ocaml"
  > EOF
  $ cat > app/melange_only.ml <<EOF
  > let x = "melange"
  > EOF

  $ dune build _build/default/app/.app.objs/byte/app__Ocaml_only.cmi \
  >   _build/default/app/.app.objs/melange/app__Melange_only.cmi

  $ test -f _build/default/app/.app.objs/byte/app__Melange_only.cmi || \
  >   echo "no byte melange_only"
  no byte melange_only
  $ test -f _build/default/app/.app.objs/melange/app__Ocaml_only.cmi || \
  >   echo "no melange ocaml_only"
  no melange ocaml_only
