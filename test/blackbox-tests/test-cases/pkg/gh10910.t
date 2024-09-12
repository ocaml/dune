This issue demonstrates a bug when we have a context which specifies a lock dir
in the `dune-workspace` file and try to lock it using the same context. It
should pick the right context but instead it lock all the directories.

  $ . ./helpers.sh

Create a fake repository with a fake package
  $ mkrepo

  $ mkpkg foo << EOF
  > install: [ "echo" "foo" ]
  > EOF

  $ cat > dune-workspace << EOF
  > (lang dune 3.16)
  > (lock_dir
  >  (path dune.lock)
  >  (repositories mock))
  > (lock_dir
  >  (path foo.lock)
  >  (repositories mock))
  > (context
  >  (default
  >   (name foo)
  >   (lock_dir foo.lock)))
  > (repository
  >  (name mock)
  >  (source "file://$(pwd)/mock-opam-repository"))
  > EOF

  $ cat > dune-project << EOF
  > (lang dune 3.16)
  > (package
  >  (name test)
  >  (allow_empty)
  >  (depends foo))
  > EOF

It tries to lock `dune.lock` whereas it should lock `foo.lock` as specified in
the `dune-workspace` file:
  $ dune pkg lock -x foo
  Solution for dune.lock:
  - foo.0.0.1

