  $ touch foo.opam
  $ cat > dune-project<<EOF
  > (lang dune 2.0)
  > EOF
  $ cat > dune <<EOF
  > (library (name foo) (public_name foo) (enabled_if false))
  > EOF
  $ export DUNE_BUILD_DIR=$(mktemp -d -t github4821XXXXXX);
  > dune build @install;
  > cat $DUNE_BUILD_DIR/default/foo.install | sed s#$DUNE_BUILD_DIR#DUNE_BUILD_DIR#g;
  lib: [
    "DUNE_BUILD_DIR/install/default/lib/foo/META"
    "DUNE_BUILD_DIR/install/default/lib/foo/dune-package"
    "DUNE_BUILD_DIR/install/default/lib/foo/opam"
  ]
