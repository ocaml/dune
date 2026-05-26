Test that (deps (package ...)) adds lib/toplevel/ to OCAMLTOP_INCLUDE_PATH.

  $ cat >dune-project <<EOF
  > (lang dune 3.24)
  > (package (name mypkg))
  > EOF
  $ mkdir src
  $ cat >src/dune <<EOF
  > (library (public_name mypkg))
  > EOF
  $ cat >src/mypkg.ml <<'EOF'
  > let x = 1
  > EOF
  $ cat >dune <<'EOF'
  > (rule
  >  (deps (package mypkg))
  >  (action (with-stdout-to out (bash "echo $OCAMLTOP_INCLUDE_PATH"))))
  > EOF

  $ baseline=$OCAMLTOP_INCLUDE_PATH
  $ dune build out
  $ env_added "$(cat _build/default/out)" "$baseline" | censor
  $PWD/_build/install/default/.packages/$DIGEST/lib/toplevel
