Failing to hash an unsupported repository entry must not change the digest of
an unrelated rule.

  $ export DUNE_TRACE=+cache
  $ mkrepo
  $ mkfifo mock-opam-repository/unsupported
  $ mkdir project
  $ cd project
  $ cat > dune-project <<'EOF'
  > (lang dune 3.26)
  > EOF
  $ cat > dune-workspace <<EOF
  > (lang dune 3.26)
  > (pkg enabled)
  > (lock_dir)
  > (lock_dir (path bad.lock) (repositories mock))
  > (repository (name mock) (url "file://$PWD/../mock-opam-repository"))
  > EOF
  $ mkdir dune.lock
  $ echo '(lang package 0.1)' > dune.lock/lock.dune
  $ echo '(rule (target completed) (action (write-file %{target} done)))' > dune
  $ dune build _build/_private/default/.lock/bad.lock completed > failed-output 2>&1
  [1]
  $ grep "^Error: Can't digest path" failed-output
  Error: Can't digest path
  $ misses() {
  >   dune trace cat | jq -cs '[.[]
  >     | select(.cat == "cache" and .name == "workspace_local_miss")
  >     | select(.args.target | endswith("/completed"))
  >     | .args.reason | split(":")[0]]'
  > }
  $ misses
  ["never seen this target before"]

The completed rule has no changed inputs and stays cached.

  $ dune build completed
  $ misses
  []
