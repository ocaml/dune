Dynamic actions currently rerun in a fresh workspace even with a shared cache.

  $ export DUNE_CACHE_ROOT="$PWD/.cache"
  $ export DUNE_CACHE=enabled
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
  $ cp -R template first
  $ (cd first && dune build --root . output && show_runs)
  {"prog":"foo.exe","process_args":["read","input"],"exit":0}
  $ cp -R template second
  $ (cd second && dune build --root . output && show_runs)
  {"prog":"foo.exe","process_args":["read","input"],"exit":0}
  $ cat second/_build/default/output
  first
