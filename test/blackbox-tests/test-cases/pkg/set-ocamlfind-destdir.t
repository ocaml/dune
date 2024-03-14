Test that the OCAMLFIND_DESTDIR environment variable is set when running
install and build commands.
  $ . ./helpers.sh

  $ make_lockdir
  $ cat >dune.lock/test.pkg <<'EOF'
  > (version 0.0.1)
  > (build (run sh -c "echo [build] OCAMLFIND_DESTDIR=$OCAMLFIND_DESTDIR"))
  > (install (run sh -c "echo [install] OCAMLFIND_DESTDIR=$OCAMLFIND_DESTDIR"))
  > EOF

  $ build_pkg test 2>&1 | sed "s#$(pwd)#PWD#" | sed 's#\.sandbox/.*/_private#\.sandbox/SANDBOX/_private#'
  [build] OCAMLFIND_DESTDIR=PWD/_build/.sandbox/SANDBOX/_private/default/.pkg/test/target/lib
  [install] OCAMLFIND_DESTDIR=PWD/_build/.sandbox/SANDBOX/_private/default/.pkg/test/target/lib
