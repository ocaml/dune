`--sandbox-actions` prevents the worker from writing to the launching dune's
shared cache.

  $ make_dune_project 3.23
  $ export DUNE_CACHE_ROOT=$PWD/cache-root
  $ echo "$DUNE_CACHE_ROOT" > cache-root-name
  $ mkdir -p "$DUNE_CACHE_ROOT/db"
  $ cat > dune <<'EOF'
  > (rule
  >  (target result)
  >  (deps cache-root-name)
  >  (action
  >   (bash
  >    "if touch \"$DUNE_CACHE_ROOT/db/runner-marker\" 2>/dev/null; then echo wrote > %{target}; else echo blocked > %{target}; fi")))
  > EOF

Normal actions can still write there.

  $ dune build result
  $ cat _build/default/result
  wrote
  $ test -e "$DUNE_CACHE_ROOT/db/runner-marker" && echo present
  present

Sandboxed actions are blocked from writing there.

  $ rm -f "$DUNE_CACHE_ROOT/db/runner-marker"
  $ dune build --sandbox-actions result
  $ cat _build/default/result
  blocked
  $ test -e "$DUNE_CACHE_ROOT/db/runner-marker" && echo present || echo missing
  missing

The Landlock backend enforces the same restriction when it is available.

  $ rm -f "$DUNE_CACHE_ROOT/db/runner-marker"
  $ if dune internal with-landlock -- true >/dev/null 2>&1; then
  >   dune build --sandbox-actions --sandbox-actions-backend=landlock result
  >   cat _build/default/result
  >   test -e "$DUNE_CACHE_ROOT/db/runner-marker" && echo present || echo missing
  > else
  >   echo blocked
  >   echo missing
  > fi
  blocked
  missing

When Landlock is available, the automatic backend does not require a working
bubblewrap binary.

  $ rm -f "$DUNE_CACHE_ROOT/db/runner-marker"
  $ if dune internal with-landlock -- true >/dev/null 2>&1; then
  >   mkdir -p fake-bin
  >   cat > fake-bin/bwrap <<'EOF'
  > #!/bin/sh
  > echo broken bwrap >&2
  > exit 1
  > EOF
  >   chmod +x fake-bin/bwrap
  >   PATH=$PWD/fake-bin:$PATH dune build --sandbox-actions result
  >   cat _build/default/result
  >   test -e "$DUNE_CACHE_ROOT/db/runner-marker" && echo present || echo missing
  > else
  >   echo blocked
  >   echo missing
  > fi
  blocked
  missing

The bubblewrap backend is still available explicitly.

  $ rm -f "$DUNE_CACHE_ROOT/db/runner-marker"
  $ dune build --sandbox-actions --sandbox-actions-backend=bwrap result
  $ cat _build/default/result
  blocked
  $ test -e "$DUNE_CACHE_ROOT/db/runner-marker" && echo present || echo missing
  missing

If the shared cache path does not exist yet, dune creates it and still protects
it from the worker.

  $ export DUNE_CACHE_ROOT=$PWD/fresh-cache-root
  $ echo "$DUNE_CACHE_ROOT" > cache-root-name
  $ test -e "$DUNE_CACHE_ROOT/db" && echo present || echo missing
  missing
  $ dune build --sandbox-actions result
  $ cat _build/default/result
  blocked
  $ test -d "$DUNE_CACHE_ROOT/db" && echo present || echo missing
  present
  $ test -e "$DUNE_CACHE_ROOT/db/runner-marker" && echo present || echo missing
  missing
