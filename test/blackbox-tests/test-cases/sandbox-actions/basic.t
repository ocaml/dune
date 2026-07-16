`--sandbox-actions` only invalidates stale outputs for actions that run
processes.

  $ make_dune_project 3.23
  $ export DUNE_TRACE=action
  $ cat > dune <<'EOF'
  > (rule
  >  (target pure)
  >  (action (write-file %{target} pure)))
  > 
  > (rule
  >  (target probe)
  >  (action (bash "readlink /proc/self/ns/mnt > %{target}")))
  > EOF
  $ readlink /proc/self/ns/mnt > host-ns

  $ dune build pure probe
  $ dune trace cat | jq -s 'include "dune"; writeFileCountBySuffix("/pure")'
  1
  $ cmp -s host-ns _build/default/probe && echo same || echo different
  same
  $ cat _build/default/pure
  pure

  $ dune build --sandbox-actions pure probe
  $ dune trace cat | jq -s 'include "dune"; writeFileCountBySuffix("/pure")'
  0
  $ if dune internal with-landlock -- true >/dev/null 2>&1; then
  >   cmp -s host-ns _build/default/probe && echo expected || echo unexpected
  > else
  >   cmp -s host-ns _build/default/probe && echo unexpected || echo expected
  > fi
  expected
  $ cat _build/default/pure
  pure

  $ dune build pure probe
  $ dune trace cat | jq -s 'include "dune"; writeFileCountBySuffix("/pure")'
  0
  $ cmp -s host-ns _build/default/probe && echo same || echo different
  same
  $ cat _build/default/pure
  pure
