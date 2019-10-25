Check that while we refuse to cache symlinks, subsequent promotions still work.

  $ cat > dune-project <<EOF
  > (lang dune 2.0)
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
  $ env DUNE_CACHE=1 DUNE_CACHE_MODE=direct DUNE_CACHE_EXIT_NO_CLIENT=1 XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune build target
  $ ./stat.sh --format=%h _build/default/source
  2
  $ ./stat.sh --format=%h _build/default/target
  2
