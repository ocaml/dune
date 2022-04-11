Check that: (i) we refuse to cache symbolic links, and (ii) rules that use the
produced symbolic links work correctly and are appropriately cached.

  $ export DUNE_CACHE=enabled
  $ export DUNE_CACHE_ROOT=$PWD/.cache

  $ cat > dune-project <<EOF
  > (lang dune 2.1)
  > EOF
  $ cat > dune <<EOF
  > (rule
  >   (deps source)
  >   (targets link)
  >   (action (bash "ln -s source link")))
  > (rule
  >   (deps link)
  >   (targets target)
  >   (action (bash "cat link link > target")))
  > EOF
  $ cat > source <<EOF
  > \_o< COIN
  > EOF
  $ dune build target

Dune cache contains entries for [source] and [target] but not for [link]

  $ (cd "$DUNE_CACHE_ROOT/meta/v5"; grep -rs . -e 'source' | dune_cmd count-lines)
  1
  $ (cd "$DUNE_CACHE_ROOT/meta/v5"; grep -rs . -e 'target' | dune_cmd count-lines)
  1
  $ (cd "$DUNE_CACHE_ROOT/meta/v5"; grep -rs . -e 'link' | dune_cmd count-lines)
  0

The files in the build directory are shared with the cache entries

  $ dune_cmd stat hardlinks _build/default/source
  2
  $ dune_cmd stat hardlinks _build/default/target
  2
