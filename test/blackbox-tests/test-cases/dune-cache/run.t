  $ cat > dune-project <<EOF
  > (lang dune 2.0)
  > EOF
  $ cat > dune <<EOF
  > (rule
  >   (deps source)
  >   (targets target)
  >   (action (bash "touch beacon ; cat source source > target")))
  > EOF
  $ cat > source <<EOF
  > \_o< COIN
  > EOF
  $ env DUNE_CACHE=1 DUNE_CACHE_EXIT_NO_CLIENT=1 XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune build target
  $ stat --format=%h _build/default/source
  2
  $ stat --format=%h _build/default/target
  2
  $ ls _build/default/beacon
  _build/default/beacon
  $ rm -rf _build/default
  $ env DUNE_CACHE=1 DUNE_CACHE_EXIT_NO_CLIENT=1 XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune build target
  $ stat --format=%h _build/default/source
  2
  $ stat --format=%h _build/default/target
  2
  $ test -e _build/default/beacon
  [1]
  $ cat _build/default/source
  \_o< COIN
  $ cat _build/default/target
  \_o< COIN
  \_o< COIN
