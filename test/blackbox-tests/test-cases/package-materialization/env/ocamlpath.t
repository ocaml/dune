Test that (deps (package ...)) adds the package's lib/ to OCAMLPATH.

  $ make_mypkg_lib_project
  $ cat >dune <<'EOF'
  > (rule
  >  (deps (package mypkg))
  >  (action (with-stdout-to out (bash "echo $OCAMLPATH"))))
  > EOF

  $ baseline=$OCAMLPATH
  $ dune build out
  $ env_added "$(cat _build/default/out)" "$baseline" | censor
  $PWD/_build/install/default/.packages/$DIGEST/lib
