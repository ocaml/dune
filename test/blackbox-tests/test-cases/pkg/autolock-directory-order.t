Recreating identical repository directories must not rerun auto-locking.
Verify that the lock and package stay cached, while a real recipe edit still
invalidates both.

  $ export DUNE_TRACE=+cache
  $ mkrepo
  $ mkpkg foo 1 <<'EOF'
  > build: [ [ "true" ] ]
  > EOF
  $ mkpkg bar <<'EOF'
  > EOF

Include symlinks to exercise their executable-bit hashing as well as regular
file and directory hashing. The solver ignores these metadata links.

  $ ln -s opam "$mock_packages/foo/foo.1/metadata"
  $ ln -s opam "$mock_packages/bar/bar.0.0.1/metadata"
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
  $ mv mock-opam-repository/packages saved-packages
  $ mkdir mock-opam-repository/packages
  $ for package in bar foo; do
  >   cp -RP "saved-packages/$package" mock-opam-repository/packages/
  > done
  $ diff -r saved-packages mock-opam-repository/packages
  $ dune build @pkg-install > reordered-output 2>&1
  $ misses
  {"lock":0,"package":0}
  $ diff -r _build/lock.snapshot _build/_private/default/.lock/dune.lock
  $ dune build @pkg-install > final-output 2>&1
  $ misses
  {"lock":0,"package":0}

A real change to the package's recipe must still relock and rebuild it.

  $ mkpkg foo 1 <<'EOF'
  > build: [ [ "sh" "-c" "true" ] ]
  > EOF
  $ dune build @pkg-install > changed-output 2>&1
  $ misses
  {"lock":1,"package":1}
  $ dune build @pkg-install > unchanged-output 2>&1
  $ misses
  {"lock":0,"package":0}
