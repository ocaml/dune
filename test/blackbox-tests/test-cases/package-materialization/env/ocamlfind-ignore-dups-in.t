Test that (deps (package ...)) adds lib/ to OCAMLFIND_IGNORE_DUPS_IN.

  $ make_mypkg_lib_project
  $ cat >dune <<'EOF'
  > (rule
  >  (deps (package mypkg))
  >  (action (with-stdout-to out (bash "echo $OCAMLFIND_IGNORE_DUPS_IN"))))
  > EOF

  $ baseline=$OCAMLFIND_IGNORE_DUPS_IN
  $ dune build out
  $ env_added "$(cat _build/default/out)" "$baseline" | censor
  $PWD/_build/install/default/.packages/$DIGEST/lib
