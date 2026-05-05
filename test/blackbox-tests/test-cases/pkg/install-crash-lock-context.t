Reproduce the crash reported in #14272: `dune install` with a lock-based context
crashes because it calls `Build_system.build_dir` outside `Build_system.run` by
`Context.roots`, where the build state is still `Initializing` rather than
`Building _`.

  $ mkrepo

Create a minimal project with a package.

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > (package
  >  (name mypkg)
  >  (allow_empty))
  > EOF

Create a lock dir with a fake package so that reading it exercises the rule
engine.

  $ make_lockdir
  $ make_lockpkg foo <<EOF
  > (version 0.0.1)
  > (install
  >  (run echo "installing foo"))
  > EOF

Build the lockdir first

  $ dune build @install

`dune install` with a --prefix argument doesn't crash.

  $ dune install --prefix "$PWD/_install"
  Error: dune install is not supported with Dune package management.
  Hint: Use 'opam' instead.
  [1]

But `dune install` without a prefix argument crashes with an internal error.

  $ dune install 2>&1 | head -n 6
  Error: dune install is not supported with Dune package management.
  Hint: Use 'opam' instead.
  [1]



Autolocking case: no source lock directory, lock dir is auto-generated into
the build tree. `dune install` must read it from there.

  $ rm -rf dune.lock

  $ add_mock_repo_if_needed

  $ enable_pkg

  $ dune build @install

Again, `dune install` with prefix doesn't crash, but without a prefix argument
crashes with the same error.

  $ dune install --prefix "$PWD/_install"
  Error: dune install is not supported with Dune package management.
  Hint: Use 'opam' instead.
  [1]

  $ dune install 2>&1 | head -n 6
  Error: dune install is not supported with Dune package management.
  Hint: Use 'opam' instead.
  [1]
