Check that missing data files simply make the cached rule invalid.

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
  $ env XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune build --config-file=config target
  $ rm -rf _build $PWD/.xdg-cache/dune/db/files/v3/
  $ env XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune build --config-file=config target
