Reproduces a crash where `dune install` from #14272

The case when `dune install` crashes is this: there is already a lock_directory
in `dune.lock`. First run `dune build @install` to generate build artifacts.
Later when you run `dune install`, it runs under
`Scheduler_setup.no_build_no_rpc` and doesn't call `Build_system.run`. The build
system state stays at `Initializing`. However, when Build_system.build_dir is
called on the lock directory, it is expecting the state to be in `Building _`
and it fails.

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

Build the lockdir first, so the install command needs to read real contents.

  $ dune build @install

`dune install` crashes with an internal error because it never calls
`Build_system.run`, yet it's expected to be in `Building` state.

  $ dune install 2>&1 | head -n 6
  Internal error! Please report to https://github.com/ocaml/dune/issues,
  providing the file _build/trace.csexp, if possible. This includes build
  commands, message logs, and file paths.
  Description:
    ("Unexpected build progress state (expected [Building _])",
     { current = Initializing })
  [1]
