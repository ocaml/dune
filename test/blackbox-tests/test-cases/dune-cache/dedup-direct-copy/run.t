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
  >   (action (copy source target)))
  > EOF
  $ cat > source <<EOF
  > \_o< COIN
  > EOF

Build target, which is a copy of source. Dune will actually copy the
file, and submit it to the cache for promotion. At this point the
respective hardlink count of "source" and "target" is 2 and 1. Since
it is already cached (as "source"), the daemon will instead send a
deduplication message. Upon reception dune should replace "target"
with a hardlink to the cache (and thus also "source"), upping the
hardlink counts to 3 and 3.

  $ env XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune build --config-file config target
  $ dune_cmd stat hardlinks _build/default/source
  1
  $ dune_cmd stat hardlinks _build/default/target
  1
