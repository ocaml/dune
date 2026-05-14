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

Dune cache contains an entry for [target] but not for the source-copy primitive
or the symbolic link.

  $ (cd "$DUNE_CACHE_ROOT/db/meta/v5"; grep -rs . -e 'source' | dune_cmd count-lines)
  0
  [1]
  $ (cd "$DUNE_CACHE_ROOT/db/meta/v5"; grep -rs . -e 'target' | dune_cmd count-lines)
  1
  $ (cd "$DUNE_CACHE_ROOT/db/meta/v5"; grep -rs . -e 'link' | dune_cmd count-lines)
  0
  [1]

The generated target is shared with its cache entry

  $ dune_cmd stat hardlinks _build/default/target
  2
