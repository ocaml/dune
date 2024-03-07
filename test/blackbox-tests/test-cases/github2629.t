  $ touch foo.ml
  $ cat > dune-project<<EOF
  > (lang dune 1.1)
  > (package (name foo))
  > EOF
  $ cat > dune <<EOF
  > (executable (public_name foo))
  > EOF
  $ export DUNE_BUILD_DIR=$(mktemp -d -t github2629XXXXXX);
  > dune build @install;
  > cat $DUNE_BUILD_DIR/default/foo.install | sed s#$DUNE_BUILD_DIR#DUNE_BUILD_DIR#g;
  > dune install --dry-run --prefix lib/foo
  lib: [
    "DUNE_BUILD_DIR/install/default/lib/foo/META"
    "DUNE_BUILD_DIR/install/default/lib/foo/dune-package"
  ]
  bin: [
    "DUNE_BUILD_DIR/install/default/bin/foo"
  ]
