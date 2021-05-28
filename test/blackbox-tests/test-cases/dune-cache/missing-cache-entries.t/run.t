Check that Dune cache can cope with missing file/metadata entries.

  $ export DUNE_CACHE_ROOT=$PWD/.cache

  $ cat > config <<EOF
  > (lang dune 2.1)
  > (sandboxing_preference none)
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
  > (rule
  >   (deps source)
  >   (targets twin-a twin-b)
  >   (action (bash "echo twin-a-contents >> twin-a; echo twin-b-contents >> twin-b")))
  > EOF
  $ cat > source <<EOF
  > \_o< COIN
  > EOF

  $ dune build --config-file=config target twin-a twin-b
  $ cat _build/default/target
  \_o< COIN
  \_o< COIN
  $ cat _build/default/twin-a _build/default/twin-b
  twin-a-contents
  twin-b-contents

Delete the [files/v4] storage and test that Dune can cope with this.

  $ rm -rf _build "$DUNE_CACHE_ROOT"/files/v4/
  $ dune build --config-file=config target
  $ cat _build/default/target
  \_o< COIN
  \_o< COIN

Same but for the [meta/v5] storage.

  $ rm -rf _build "$DUNE_CACHE_ROOT"/meta/v5/
  $ dune build --config-file=config target
  $ cat _build/default/target
  \_o< COIN
  \_o< COIN

Now nuke the whole cache directory.

  $ rm -rf "$DUNE_CACHE_ROOT"
  $ rm -rf _build
  $ dune build --config-file=config target twin-a twin-b
  $ cat _build/default/target
  \_o< COIN
  \_o< COIN

Selectively delete just one of the set of targets.

  $ twin_b_entry=$(dune_cmd find-file-by-contents-regexp "$DUNE_CACHE_ROOT" "twin-b-contents")
  $ rm "$twin_b_entry"
  $ rm -r _build

  $ dune build --config-file=config twin-a
  $ cat _build/default/twin-a
  twin-a-contents
  $ cat _build/default/twin-b
  twin-b-contents
