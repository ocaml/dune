Set lock file per context.

TODO: versioning will be added once this feature is stable

  $ cat >dune-workspace <<EOF
  > (lang dune 3.21)
  > (context
  >  (default
  >   (lock_dir foo.lock)))
  > (context
  >  (default
  >   (name foo)
  >   (lock_dir bar.lock)))
  > EOF

  $ mkdir foo.lock
  $ cat >foo.lock/lock.dune <<EOF
  > (lang package 0.1)
  > (repositories (complete true))
  > EOF
  $ cat >foo.lock/test.pkg <<EOF
  > (version 0.0.1)
  > (build
  >  (system "echo building from %{context_name}"))
  > EOF

  $ mkdir bar.lock
  $ cat >bar.lock/lock.dune <<EOF
  > (lang package 0.1)
  > (repositories (complete true))
  > EOF
  $ cat >bar.lock/test.pkg <<EOF
  > (version 0.0.2)
  > (build
  >  (system "echo building from %{context_name}"))
  > EOF

  $ dune build @@_build/default/pkg-install
  building from default

  $ dune build @@_build/foo/pkg-install
  building from foo
