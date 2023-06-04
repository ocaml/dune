Set lock file per context.

TODO: versioning will be added once this feature is stable

  $ cat >dune-workspace <<EOF
  > (lang dune 3.8)
  > (context
  >  (default
  >   (lock foo.lock)))
  > (context
  >  (default
  >   (name foo)
  >   (lock bar.lock)))
  > EOF

  $ mkdir foo.lock
  $ cat >foo.lock/lock.dune <<EOF
  > (lang package 0.1)
  > EOF
  $ cat >foo.lock/test <<EOF
  > (build
  >  (system "echo building from %{context_name}"))
  > EOF
  $ ln -s foo.lock bar.lock

  $ dune build .pkg/test/target
  building from foo
  building from default
