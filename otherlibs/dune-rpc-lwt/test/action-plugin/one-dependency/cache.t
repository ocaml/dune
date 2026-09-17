A fresh workspace replays dependency discovery before restoring dynamic outputs.
The cache trace reports discovery misses as well as artifact misses.

  $ export DUNE_CACHE_ROOT="$PWD/.cache"
  $ export DUNE_CACHE=enabled
  $ export DUNE_TRACE=+cache
  $ mkdir template
  $ cp bin/foo.exe template/
  $ cat > template/dune-project <<'EOF'
  > (lang dune 2.0)
  > (using action-plugin 0.1)
  > EOF
  $ printf first > template/source
  $ cat > template/dune <<'EOF'
  > (rule (target input) (deps source) (action (copy source input)))
  > (rule
  >  (target output)
  >  (action (with-stdout-to output (dynamic-run ./foo.exe read input))))
  > EOF
  $ show_runs () {
  >   dune trace cat | jq -c '
  >     select(.cat == "process" and .name == "finish")
  >     | select(.args.prog | endswith("foo.exe"))
  >     | .args | {prog: (.prog | split("/") | last), process_args, exit}'
  > }
  $ show_misses () {
  >   dune trace cat | jq -c '
  >     select(.cat == "cache" and .name == "miss")
  >     | select(.args.head | endswith("/output"))
  >     | {name, reason: .args.reason}'
  > }
  $ cp -R template first
  $ (cd first && dune build --root . output && show_runs)
  {"prog":"foo.exe","process_args":["read","input"],"exit":0}
  $ (cd first && show_misses)
  {"name":"miss","reason":"dynamic dependency manifest unavailable"}
  $ cp -R template second
  $ (cd second && dune build --root . output && show_runs)
  $ (cd second && show_misses)
  $ cat second/_build/default/output
  first

Changed inputs select a new branch; old branches remain reusable.

  $ printf changed > template/source
  $ cp -R template changed
  $ (cd changed && dune build --root . output && show_runs)
  {"prog":"foo.exe","process_args":["read","input"],"exit":0}
  $ cat changed/_build/default/output
  changed
  $ (cd changed && show_misses)
  {"name":"miss","reason":"dynamic dependency manifest unavailable"}
  $ cp -R template changed-again
  $ (cd changed-again && dune build --root . output && show_runs)
  $ printf first > template/source
  $ cp -R template old-branch
  $ (cd old-branch && dune build --root . output && show_runs)

A shared hit also populates the workspace cache with the dynamic dependencies.

  $ (cd second && dune build --root . output && show_runs)
  $ printf local-change > second/source
  $ (cd second && dune build --root . output && show_runs)
  {"prog":"foo.exe","process_args":["read","input"],"exit":0}
  $ cat second/_build/default/output
  local-change

Copy-mode storage works too.

  $ export DUNE_CACHE_STORAGE_MODE=copy
  $ printf copy-mode > template/source
  $ cp -R template copy-first
  $ (cd copy-first && dune build --root . output && show_runs)
  {"prog":"foo.exe","process_args":["read","input"],"exit":0}
  $ cp -R template copy-again
  $ (cd copy-again && dune build --root . output && show_runs)
  $ cat copy-again/_build/default/output
  copy-mode

Artifact lookup already reports misses for reproducibility checks.

  $ cp -R template check
  $ (cd check && dune build --root . output --cache-check-probability=1.0 && show_misses)
  {"name":"miss","reason":"rerunning for reproducibility check"}

An earlier request can select a different dependency. Replay must stop before
trying to build the now-absent input from the old branch.

  $ cat > template/dune <<'EOF'
  > (rule
  >  (target output)
  >  (action (with-stdout-to output (dynamic-run ./foo.exe choose))))
  > EOF
  $ printf left > template/choice
  $ printf left-value > template/left
  $ cp -R template choose-left
  $ (cd choose-left && dune build --root . output && show_runs)
  {"prog":"foo.exe","process_args":["choose"],"exit":0}
  $ printf right > template/choice
  $ rm template/left
  $ printf right-value > template/right
  $ cp -R template choose-right
  $ (cd choose-right && dune build --root . output && show_runs)
  {"prog":"foo.exe","process_args":["choose"],"exit":0}
  $ cat choose-right/_build/default/output
  right-value
  $ cp -R template choose-again
  $ (cd choose-again && dune build --root . output && show_runs)

Glob requests are replayed, including changes to matching files and names.

  $ cat > template/dune <<'EOF'
  > (rule (target listed-generated) (action (write-file listed-generated generated)))
  > (rule
  >  (target output)
  >  (action (with-stdout-to output (dynamic-run ./foo.exe glob . listed*))))
  > EOF
  $ printf one > template/listed-source
  $ cp -R template glob-first
  $ (cd glob-first && dune build --root . output && show_runs)
  {"prog":"foo.exe","process_args":["glob",".","listed*"],"exit":0}
  $ cp -R template glob-again
  $ (cd glob-again && dune build --root . output && show_runs)
  $ cat glob-again/_build/default/output
  listed-generated
  listed-source
  $ printf two > template/listed-source
  $ cp -R template glob-changed
  $ (cd glob-changed && dune build --root . output && show_runs)
  {"prog":"foo.exe","process_args":["glob",".","listed*"],"exit":0}
  $ printf extra > template/listed-added
  $ cp -R template glob-added
  $ (cd glob-added && dune build --root . output && show_runs)
  {"prog":"foo.exe","process_args":["glob",".","listed*"],"exit":0}
  $ cat glob-added/_build/default/output
  listed-added
  listed-generated
  listed-source

A syntactically valid payload with an invalid manifest shape is also a miss.

  $ grep -rl '^[(]4:deps' "$DUNE_CACHE_ROOT/db/files" | while read entry; do
  >   chmod u+w "$entry"
  >   printf '(7:unknown())' > "$entry"
  > done
  $ cp -R template invalid-manifest
  $ (cd invalid-manifest && dune build --root . output && show_runs)
  {"prog":"foo.exe","process_args":["glob",".","listed*"],"exit":0}
  $ (cd invalid-manifest && show_misses)
  {"name":"miss","reason":"dynamic dependency manifest unavailable"}
  $ cat invalid-manifest/_build/default/output
  listed-added
  listed-generated
  listed-source

Ordinary trimming reclaims both manifest payloads and their metadata.

  $ grep -rl '.dap-manifest' "$DUNE_CACHE_ROOT/db/meta" > /dev/null
  $ dune cache trim --size 0B > /dev/null
  $ grep -rl '.dap-manifest' "$DUNE_CACHE_ROOT/db/meta"
  [1]
  $ cp -R template trimmed
  $ (cd trimmed && dune build --root . output && show_runs)
  {"prog":"foo.exe","process_args":["glob",".","listed*"],"exit":0}
  $ (cd trimmed && show_misses)
  {"name":"miss","reason":"dynamic dependency manifest unavailable"}

Corrupt discovery metadata is a miss, even when its artifacts remain cached.

  $ grep -rl '.dap-manifest' "$DUNE_CACHE_ROOT/db/meta" | while read entry; do
  >   chmod u+w "$entry"
  >   printf broken > "$entry"
  > done
  $ cp -R template corrupt
  $ (cd corrupt && dune build --root . output && show_runs)
  {"prog":"foo.exe","process_args":["glob",".","listed*"],"exit":0}
  $ cat corrupt/_build/default/output
  listed-added
  listed-generated
  listed-source
  $ (cd corrupt && show_misses)
  {"name":"miss","reason":"dynamic dependency manifest unavailable"}

Disabled caching and sandbox exclusion also report why no shared hit is possible.

  $ cp -R template disabled
  $ (cd disabled && DUNE_CACHE=disabled dune build --root . output && show_misses)
  {"name":"miss","reason":"can't go in shared cache"}
  $ cat > template/dune <<'EOF'
  > (rule (target listed-generated) (action (write-file listed-generated generated)))
  > (rule
  >  (target output)
  >  (deps (sandbox always))
  >  (action (with-stdout-to output (dynamic-run ./foo.exe glob . listed*))))
  > EOF
  $ cp -R template sandboxed
  $ (cd sandboxed && dune build --root . output --sandbox=hardlink && show_misses)
  {"name":"miss","reason":"can't go in shared cache"}
