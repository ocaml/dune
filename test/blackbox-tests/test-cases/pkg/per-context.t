Set lock file per context.

TODO: versioning will be added once this feature is stable

  $ . ./helpers.sh

  $ cat >dune-workspace <<EOF
  > (lang dune 3.8)
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
  $ ln -s foo.lock bar.lock

  $ build_pkg test
  building from default
