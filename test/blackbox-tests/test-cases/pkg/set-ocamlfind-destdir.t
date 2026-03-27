Test that the OCAMLFIND_DESTDIR environment variable is set when running
install and build commands.

  $ make_lockdir
  $ make_lockpkg test <<'EOF'
  > (version 0.0.1)
  > (build (run sh -c "echo [build] OCAMLFIND_DESTDIR=$OCAMLFIND_DESTDIR"))
  > (install (run sh -c "echo [install] OCAMLFIND_DESTDIR=$OCAMLFIND_DESTDIR"))
  > EOF

  $ build_pkg test 2>&1 \
  > | dune_cmd subst "$PWD" PWD \
  > | dune_cmd subst '\.sandbox/.*/_private' '.sandbox/SANDBOX/_private'
  [build] OCAMLFIND_DESTDIR=PWD/_build/.sandbox/SANDBOX/_private/default/.pkg/test.0.0.1-72bb31d43e47c1ce47f5cbd1c7042f7c/target/lib
  [install] OCAMLFIND_DESTDIR=PWD/_build/.sandbox/SANDBOX/_private/default/.pkg/test.0.0.1-72bb31d43e47c1ce47f5cbd1c7042f7c/target/lib
