Test that (deps (package ...)) adds lib/toplevel/ to OCAMLTOP_INCLUDE_PATH.

  $ make_mypkg_lib_project
  $ cat >dune <<'EOF'
  > (rule
  >  (deps (package mypkg))
  >  (action (with-stdout-to out (bash "echo $OCAMLTOP_INCLUDE_PATH"))))
  > EOF

  $ baseline=$OCAMLTOP_INCLUDE_PATH
  $ dune build out
  $ env_added "$(cat _build/default/out)" "$baseline" | censor
  $PWD/_build/install/default/.packages/$DIGEST/lib/toplevel
