Recreating identical repository directories can unnecessarily rerun auto-locking.
The trace fields "lock" and "package" count cache misses for the auto-lock and
package build rules. This bug reruns auto-locking, not the package build.

  $ export DUNE_TRACE=+cache
  $ mkrepo
  $ mkpkg foo 1 <<'EOF'
  > build: [ [ "true" ] ]
  > EOF
  $ mkpkg bar <<'EOF'
  > EOF
  $ add_mock_repo_if_needed
  $ enable_pkg
  $ cat > dune-project <<'EOF'
  > (lang dune 3.26)
  > (package (name app) (allow_empty) (depends foo))
  > EOF
  $ misses() {
  >   dune trace cat | jq -cs '
  >     [ .[]
  >       | select(.cat == "cache" and .name == "workspace_local_miss")
  >       | .args.target
  >     ] as $misses
  >     | {
  >         lock: ($misses | map(select(
  >           endswith("/.lock/dune.lock"))) | length),
  >         package: ($misses | map(select(
  >           contains("foo.") and endswith("/target"))) | length)
  >       }
  >   '
  > }
  $ dune build @pkg-install > first-output 2>&1
  $ misses
  {"lock":1,"package":1}
  $ cp -R _build/_private/default/.lock/dune.lock _build/lock.snapshot
  $ dune build @pkg-install > repeated-output 2>&1
  $ misses
  {"lock":0,"package":0}

Recreate the same directories in another order, without changing any contents.
If enumeration order changes, the bug causes one lock miss but no package miss.
If the filesystem preserves the order, zero misses are only a control: that run
does not exercise the bug.

  $ find mock-opam-repository/packages/. ! -name . -prune -print > order.before
  $ mv mock-opam-repository/packages saved-packages
  $ mkdir mock-opam-repository/packages
  $ for package in bar foo; do
  >   cp -R "saved-packages/$package" mock-opam-repository/packages/
  > done
  $ find mock-opam-repository/packages/. ! -name . -prune -print > order.after
  $ if cmp -s order.before order.after; then
  >   echo '{"lock":0,"package":0}' > expected-misses
  > else
  >   echo '{"lock":1,"package":0}' > expected-misses
  > fi
  $ diff -r saved-packages mock-opam-repository/packages
  $ dune build @pkg-install > reordered-output 2>&1
  $ misses | diff -u expected-misses -
  $ diff -r _build/lock.snapshot _build/_private/default/.lock/dune.lock
  $ dune build @pkg-install > final-output 2>&1
  $ misses
  {"lock":0,"package":0}
