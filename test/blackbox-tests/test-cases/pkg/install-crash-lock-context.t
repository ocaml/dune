Regression test for #14272: `dune install` with a lock-based context used to
crash because it called `Build_system.build_dir` outside `Build_system.run`,
where the build state is `Initializing` rather than `Building _`. It shouldn't
crash now, but the test is to provide coverage for this code path.

  $ mkrepo

Create a minimal project with a package.

  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > (package
  >  (name mypkg)
  >  (allow_empty))
  > EOF

Enable package management so that the default context becomes lock-based.

  $ cat > dune-workspace <<EOF
  > (lang dune 3.20)
  > (pkg enabled)
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

`dune install` without --prefix calls `Context.roots`, which triggers the lock
dir read. We unset OPAM_SWITCH_PREFIX for a deterministic result.

  $ dune install
  Error: The mandir installation directory is unknown.
  Hint: It can be specified with '--prefix' or by setting '--mandir'
  [1]
  $ dune install --prefix "$PWD/_install"

Autolocking case: no source lock directory, lock dir is auto-generated into
the build tree. `dune install` must read it from there.

  $ mkdir autolock_test
  $ cd autolock_test

  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > (package
  >  (name mypkg)
  >  (allow_empty))
  > EOF

  $ cat > dune-workspace <<EOF
  > (lang dune 3.20)
  > (pkg enabled)
  > EOF

  $ mkrepo
  $ add_mock_repo_if_needed

  $ dune build @install

  $ dune install
  Error: The mandir installation directory is unknown.
  Hint: It can be specified with '--prefix' or by setting '--mandir'
  [1]
  $ dune install --prefix "$PWD/_install"
