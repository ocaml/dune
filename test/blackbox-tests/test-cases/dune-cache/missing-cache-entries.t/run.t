Check that Dune cache can cope with missing file/metadata entries.

  $ export DUNE_CACHE_ROOT=$PWD/.cache

  $ cat > config <<EOF
  > (lang dune 2.1)
  > (cache enabled)
  > (cache-duplication copy)
  > (cache-transport direct)
  > EOF
  $ cat > dune-project <<EOF
  > (lang dune 2.1)
  > EOF
  $ cat > dune <<EOF
  > (rule
  >   (deps source)
  >   (targets target)
  >   (action (bash "cat source source > target")))
  > EOF
  $ cat > source <<EOF
  > \_o< COIN
  > EOF

  $ dune build --config-file=config target
  $ cat _build/default/target
  \_o< COIN
  \_o< COIN

Delete the [files/v4] storage and test that Dune can cope with this.

  $ rm -rf _build $DUNE_CACHE_ROOT/files/v4/
  $ dune build --config-file=config target
  $ cat _build/default/target
  \_o< COIN
  \_o< COIN

Same but for the [meta/v5] storage.

  $ rm -rf _build $DUNE_CACHE_ROOT/meta/v5/
  $ dune build --config-file=config target
  $ cat _build/default/target
  \_o< COIN
  \_o< COIN

Now nuke the whole cache directory.

  $ rm -rf $DUNE_CACHE_ROOT
  $ dune build --config-file=config target
  $ cat _build/default/target
  \_o< COIN
  \_o< COIN
