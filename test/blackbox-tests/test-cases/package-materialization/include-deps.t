(package ...) inside (include ...) deps is processed: the package's
layout lib/ dir gets added to the action's OCAMLPATH alongside outer
(package ...) entries.

  $ cat >dune-project <<EOF
  > (lang dune 3.24)
  > (package (name outer))
  > (package (name inner))
  > EOF

  $ mkdir outer-src inner-src

  $ cat >outer-src/dune <<EOF
  > (library (public_name outer))
  > EOF
  $ cat >outer-src/outer.ml <<EOF
  > let x = 1
  > EOF

  $ cat >inner-src/dune <<EOF
  > (library (public_name inner))
  > EOF
  $ cat >inner-src/inner.ml <<EOF
  > let x = 2
  > EOF

  $ cat >inner-deps.sexp <<'EOF'
  > ((package inner))
  > EOF

  $ cat >dune <<'EOF'
  > (rule
  >  (deps (package outer) (include inner-deps.sexp))
  >  (action
  >   (with-stdout-to out
  >    (bash "echo $OCAMLPATH"))))
  > EOF

  $ baseline=$OCAMLPATH
  $ dune build out

The action's OCAMLPATH includes the outer and the included package's
layout lib/ dirs:

  $ env_added "$(cat _build/default/out)" "$baseline" | censor
  $PWD/_build/install/default/.packages/$DIGEST1/lib
  $PWD/_build/install/default/.packages/$DIGEST2/lib
