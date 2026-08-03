Test that (deps (package ...)) adds lib/stublibs/ to CAML_LD_LIBRARY_PATH.

  $ make_mypkg_stubs_project
  $ cat >dune <<'EOF'
  > (rule
  >  (deps (package mypkg))
  >  (action (with-stdout-to out (bash "echo $CAML_LD_LIBRARY_PATH"))))
  > EOF

  $ baseline=$CAML_LD_LIBRARY_PATH
  $ dune build out
  $ env_added "$(cat _build/default/out)" "$baseline" | censor
  $PWD/_build/install/default/.packages/$DIGEST/lib/stublibs
