Dune uses Landlock, when available, to confine sandboxed build actions.

  $ rm -rf _build cache-root outside
  $ make_dune_project 3.24
  $ export DUNE_CACHE_ROOT=$PWD/cache-root
  $ export OUTSIDE=$PWD/outside
  $ mkdir -p "$DUNE_CACHE_ROOT/db" "$OUTSIDE"
  $ cat > try-writes.sh <<'EOF'
  > try_touch () {
  >   if touch "$1" 2>/dev/null; then echo "$2-wrote"; else echo "$2-blocked"; fi
  > }
  > if : > "$1"; then echo target-wrote >> "$1"; else echo target-blocked; fi
  > if printf ignored > /dev/null; then echo null-wrote >> "$1"; else echo null-blocked >> "$1"; fi
  > if head -c 1 /dev/zero >/dev/null && printf ignored > /dev/zero; then
  >   echo dev-wrote >> "$1"
  > else
  >   echo dev-blocked >> "$1"
  > fi
  > if mkdir -p "$DUNE_ACTION_TRACE_DIR" 2>/dev/null && touch "$DUNE_ACTION_TRACE_DIR/$2.json"; then
  >   echo trace-wrote >> "$1"
  > else
  >   echo trace-blocked >> "$1"
  > fi
  > if test -d /dev/shm; then
  >   shm_path="/dev/shm/dune-landlock-$2-$$"
  >   try_touch "$shm_path" shm >> "$1"
  >   rm -f "$shm_path" 2>/dev/null
  > else
  >   echo shm-wrote >> "$1"
  > fi
  > try_touch "$DUNE_CACHE_ROOT/db/$2" cache >> "$1"
  > try_touch "$OUTSIDE/$2" outside >> "$1"
  > try_touch "$TMPDIR/$2" tmp >> "$1"
  > tmp_parent_path="${TMPDIR%/*}/dune-landlock-$2-$$"
  > try_touch "$tmp_parent_path" tmp-parent >> "$1"
  > rm -f "$tmp_parent_path" 2>/dev/null
  > EOF
  $ cat > dune <<'EOF'
  > (rule
  >  (target unsandboxed)
  >  (deps try-writes.sh)
  >  (action (run sh %{dep:try-writes.sh} %{target} unsandboxed)))
  > (rule
  >  (target sandboxed)
  >  (deps try-writes.sh (sandbox always))
  >  (action (run sh %{dep:try-writes.sh} %{target} sandboxed)))
  > (rule
  >  (target sandboxed-disabled)
  >  (deps try-writes.sh (sandbox always))
  >  (action (run sh %{dep:try-writes.sh} %{target} sandboxed-disabled)))
  > (rule
  >  (target action-runner)
  >  (deps try-writes.sh (sandbox always))
  >  (action (run sh %{dep:try-writes.sh} %{target} action-runner)))
  > EOF

Unsandboxed actions are not restricted.

  $ dune build --sandbox=none unsandboxed
  $ cat _build/default/unsandboxed
  target-wrote
  null-wrote
  dev-wrote
  trace-wrote
  shm-wrote
  cache-wrote
  outside-wrote
  tmp-wrote
  tmp-parent-wrote

Projects using versions of the Dune language older than 3.25 do not restrict
sandboxed actions with Landlock.

  $ dune build sandboxed
  $ cat _build/default/sandboxed
  target-wrote
  null-wrote
  dev-wrote
  trace-wrote
  shm-wrote
  cache-wrote
  outside-wrote
  tmp-wrote
  tmp-parent-wrote

Changing the language version invalidates the previous result and enables the
policy.

  $ rm -f "$DUNE_CACHE_ROOT/db/sandboxed" "$OUTSIDE/sandboxed" "$TMPDIR/sandboxed"
  $ make_dune_project 3.25

Sandboxed actions can write to their sandbox and temporary directory and can
read and write devices under /dev, but cannot write elsewhere.

  $ if dune internal with-landlock -- true >/dev/null 2>&1; then
  >   dune build sandboxed
  >   cat _build/default/sandboxed
  > else
  >   echo target-wrote
  >   echo null-wrote
  >   echo dev-wrote
  >   echo trace-wrote
  >   echo shm-wrote
  >   echo cache-blocked
  >   echo outside-blocked
  >   echo tmp-wrote
  >   echo tmp-parent-blocked
  > fi
  target-wrote
  null-wrote
  dev-wrote
  trace-wrote
  shm-wrote
  cache-blocked
  outside-blocked
  tmp-wrote
  tmp-parent-blocked

DUNE_CONFIG__LANDLOCK=disabled disables the restriction.

  $ DUNE_CONFIG__LANDLOCK=disabled dune build sandboxed-disabled
  $ cat _build/default/sandboxed-disabled
  target-wrote
  null-wrote
  dev-wrote
  trace-wrote
  shm-wrote
  cache-wrote
  outside-wrote
  tmp-wrote
  tmp-parent-wrote

Actions delegated to an action runner are not restricted with Landlock.

  $ dune build --action-runner action-runner
  $ cat _build/default/action-runner
  target-wrote
  null-wrote
  dev-wrote
  trace-wrote
  shm-wrote
  cache-wrote
  outside-wrote
  tmp-wrote
  tmp-parent-wrote
