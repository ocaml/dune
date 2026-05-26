Test that copy-mode cache trim keeps recently restored entries.

  $ export DUNE_CACHE=enabled
  $ export DUNE_CACHE_STORAGE_MODE=copy
  $ export XDG_CACHE_HOME=$PWD/.xdg-cache
  $ setup_xdg_runtime_dir

  $ cat > dune-project <<EOF
  > (lang dune 2.1)
  > EOF
  $ cat > dune <<EOF
  > (rule
  >   (targets copy_a)
  >   (action (bash "touch beacon_copy_a; printf '%10000s' a > copy_a")))
  > (rule
  >   (targets copy_b)
  >   (action (bash "touch beacon_copy_b; printf '%10000s' b > copy_b")))
  > EOF

  $ dune build copy_a
  $ dune_cmd wait-for-fs-clock-to-advance
  $ dune build copy_b
  $ rm -rf _build
  $ dune_cmd wait-for-fs-clock-to-advance
  $ dune build copy_a
  $ dune_cmd exists _build/default/beacon_copy_a
  false
  $ dune cache trim --trimmed-size 6000B > /dev/null
  $ rm -rf _build
  $ dune build copy_a copy_b
  $ dune_cmd exists _build/default/beacon_copy_a
  true
  $ dune_cmd exists _build/default/beacon_copy_b
  false
