  $ cat > dune-project <<EOF
  > (lang dune 2.0)
  > EOF
  $ cat > dune <<EOF
  > (rule
  >   (deps source)
  >   (targets target)
  >   (action (copy source target)))
  > EOF
  $ cat > source <<EOF
  > \_o< COIN
  > EOF
  $ env DUNE_CACHE=1 DUNE_CACHE_EXIT_NO_CLIENT=1 XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune build target
  $ ./stat.sh --format=%h _build/default/source
  3
  $ ./stat.sh --format=%h _build/default/target
  3
