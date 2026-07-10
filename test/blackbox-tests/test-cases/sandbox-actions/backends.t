The --sandbox-actions-backend flag selects specific sandbox backends.

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

Requesting a backend without enabling sandbox-actions is rejected.

  $ dune build --sandbox-actions-backend=landlock result
  Error: --sandbox-actions-backend requires --sandbox-actions. Either pass
  --sandbox-actions or remove --sandbox-actions-backend.
  [1]

Requesting an unavailable backend reports that backend's diagnostic.

  $ rm -rf _build/fake-bin
  $ mkdir -p _build/fake-bin
  $ ln -s "$(command -v dune)" _build/fake-bin/dune
  $ PATH=$PWD/_build/fake-bin dune build --sandbox-actions --sandbox-actions-backend=bwrap result
  Error: Sandbox backend bwrap requested via --sandbox-actions-backend but
  unavailable.
  bwrap is unavailable: install the bubblewrap package and ensure the binary is
  on PATH (Linux only)
  [1]

Requesting an installed but unusable backend reports the probe failure.

  $ rm -rf fake-bwrap-bin
  $ mkdir -p fake-bwrap-bin
  $ cat > fake-bwrap-bin/bwrap <<'EOF'
  > #!/bin/sh
  > exit 42
  > EOF
  $ chmod +x fake-bwrap-bin/bwrap
  $ PATH=$PWD/fake-bwrap-bin:$PATH dune build --sandbox-actions --sandbox-actions-backend=bwrap result
  Error: Sandbox backend bwrap requested via --sandbox-actions-backend but
  unavailable.
  bwrap is unavailable: the binary was found but failed to create a sandbox
  (exit code 42). User namespaces may be disabled.
  [1]

When bwrap is unavailable, automatic backend selection uses Landlock.
The cache protection policy blocks writes to the shared cache.

  $ rm -f "$DUNE_CACHE_ROOT/db/runner-marker"
  $ bwrap_dir=$(dirname "$(command -v bwrap 2>/dev/null || echo /nonexistent/bwrap)")
  $ without_bwrap_path=$(printf '%s' "$PATH" | tr ':' '\n' | grep -vx "$bwrap_dir" | paste -sd: -)
  $ PATH=$without_bwrap_path dune build --sandbox-actions result \
  >   && cat _build/default/result
  blocked
  $ test -e "$DUNE_CACHE_ROOT/db/runner-marker" && echo present || echo missing
  missing

Automatic backend selection also uses Landlock when bwrap is on PATH but cannot
create a sandbox.

  $ rm -f "$DUNE_CACHE_ROOT/db/runner-marker"
  $ rm -rf _build
  $ PATH=$PWD/fake-bwrap-bin:$PATH dune build --sandbox-actions result \
  >   && cat _build/default/result
  blocked
  $ test -e "$DUNE_CACHE_ROOT/db/runner-marker" && echo present || echo missing
  missing

Explicit `--sandbox-actions-backend=landlock` runs the action through
Landlock. The cache protection policy blocks writes to the shared cache.

  $ rm -f "$DUNE_CACHE_ROOT/db/runner-marker"
  $ rm -rf _build
  $ dune build --sandbox-actions --sandbox-actions-backend=landlock result \
  >   && cat _build/default/result
  blocked
  $ test -e "$DUNE_CACHE_ROOT/db/runner-marker" && echo present || echo missing
  missing
