Test the shared cache in copy mode:
- Dune skips executions of rules whose artifacts are present in the cache
- Dune copies artifacts when restoring them instead of creating hard links

  $ cat > config <<EOF
  > (lang dune 2.1)
  > (cache enabled)
  > (cache-duplication copy)
  > (cache-transport daemon)
  > EOF
  $ cat > dune-project <<EOF
  > (lang dune 2.1)
  > EOF
  $ cat > dune <<EOF
  > (rule
  >   (deps source)
  >   (targets target)
  >   (action (bash "touch rule-was-run; cat source source > target")))
  > EOF

It's a duck. It quacks. (Yes, the author of this comment didn't get it.)

  $ cat > source <<EOF
  > \_o< COIN
  > EOF

Initial build

  $ env XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune build --config-file=config target
  $ dune_cmd stat hardlinks _build/default/source
  1
  $ dune_cmd stat hardlinks _build/default/target
  1
  $ ls _build/default/rule-was-run
  _build/default/rule-was-run

Clean + rebuild: Dune should restore artifacts from the cache by copying

  $ rm -rf _build/default
  $ env XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune build --config-file=config target
  $ dune_cmd stat hardlinks _build/default/source
  1
  $ dune_cmd stat hardlinks _build/default/target
  1

The rule wasn't run:

  $ dune_cmd exists _build/default/rule-was-run
  false

The files have been restored correctly:

  $ cat _build/default/source
  \_o< COIN
  $ cat _build/default/target
  \_o< COIN
  \_o< COIN

------------------

Check that rules are rebuilt correctly when using the shared cache in copy mode

  $ cat > dune-project <<EOF
  > (lang dune 2.1)
  > EOF
  $ cat > dune-v1 <<EOF
  > (rule
  >   (targets t1)
  >   (action (bash "echo running; echo v1 > t1")))
  > (rule
  >   (deps t1)
  >   (targets t2)
  >   (action (bash "echo running; cat t1 t1 > t2")))
  > EOF
  $ cat > dune-v2 <<EOF
  > (rule
  >   (targets t1)
  >   (action (bash "echo running; echo v2 > t1")))
  > (rule
  >   (deps t1)
  >   (targets t2)
  >   (action (bash "echo running; cat t1 t1 > t2")))
  > EOF
  $ cp dune-v1 dune
  $ env XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune build --config-file=config t2
          bash t1
  running
          bash t2
  running
  $ cat _build/default/t2
  v1
  v1
  $ cp dune-v2 dune
  $ env XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune build --config-file=config t2
          bash t1
  running
          bash t2
  running
  $ cat _build/default/t2
  v2
  v2
  $ cp dune-v1 dune
  $ env XDG_RUNTIME_DIR=$PWD/.xdg-runtime XDG_CACHE_HOME=$PWD/.xdg-cache dune build --config-file=config t2
  $ cat _build/default/t1
  v1
  $ cat _build/default/t2
  v1
  v1
