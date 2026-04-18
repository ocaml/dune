`--sandbox-actions` runs actions in the external worker and invalidates
stale outputs when the mode changes.

  $ make_dune_project 3.23
  $ cat > dune <<'EOF'
  > (rule
  >  (target probe)
  >  (action (bash "readlink /proc/self/ns/mnt > %{target}")))
  > EOF
  $ readlink /proc/self/ns/mnt > host-ns

  $ dune build probe
  $ cmp -s host-ns _build/default/probe && echo same || echo different
  same

  $ dune build --sandbox-actions probe
  $ cmp -s host-ns _build/default/probe && echo same || echo different
  different

  $ dune build probe
  $ cmp -s host-ns _build/default/probe && echo same || echo different
  same
