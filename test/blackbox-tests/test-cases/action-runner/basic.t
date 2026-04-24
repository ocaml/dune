`--action-runner` does not invalidate stale outputs when it is toggled.

  $ make_dune_project 3.23
  $ export TEST_DIR=$PWD
  $ cat > dune <<'EOF'
  > (rule
  >  (target probe)
  >  (action
  >   (bash
  >    "count=0; if [ -e \"$TEST_DIR/counter\" ]; then count=$(cat \"$TEST_DIR/counter\"); fi; count=$((count + 1)); echo $count > \"$TEST_DIR/counter\"; echo ran-$count > %{target}")))
  > EOF

  $ dune build probe
  $ cat counter
  1

  $ dune build --action-runner probe
  $ cat counter
  1

  $ dune build probe
  $ cat counter
  1

