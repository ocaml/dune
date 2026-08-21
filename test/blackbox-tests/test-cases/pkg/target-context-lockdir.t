A target context must inherit the lockdir selected by its owning base context.
For a default context targeting the findlib toolchain "foo", the derived
context is named "default.foo".

First configure the base context to use foo.lock and create a distinct package
in each lockdir:

  $ mkdir fallback
  $ cd fallback
  $ cat > dune-workspace <<EOF
  > (lang dune 3.24)
  > (context
  >  (default
  >   (lock_dir foo.lock)
  >   (targets native foo)))
  > (lock_dir
  >  (path dune.lock))
  > EOF

  $ source_lock_dir=foo.lock make_lockdir
  $ source_lock_dir=foo.lock make_lockpkg test <<EOF
  > (version 0.0.1)
  > (build (run echo "building from foo.lock"))
  > EOF

  $ make_lockdir
  $ make_lockpkg test <<EOF
  > (version 0.0.2)
  > (build (run echo "building from dune.lock"))
  > EOF

The target context silently falls back to dune.lock instead of inheriting
foo.lock from the default context:

  $ dune build @@_build/default.foo/pkg-install
  building from dune.lock

In a fresh workspace with no dune.lock, a native-only build only needs the
foo.lock selected by its context:

  $ cd ..
  $ mkdir native-only
  $ cd native-only
  $ cat > dune-workspace <<EOF
  > (lang dune 3.24)
  > (context
  >  (default
  >   (lock_dir foo.lock)
  >   (targets native foo)))
  > EOF

  $ source_lock_dir=foo.lock make_lockdir
  $ source_lock_dir=foo.lock make_lockpkg test <<EOF
  > (version 0.0.1)
  > (build (run echo "building from foo.lock"))
  > EOF

  $ dune build @@_build/default/pkg-install
  building from foo.lock
