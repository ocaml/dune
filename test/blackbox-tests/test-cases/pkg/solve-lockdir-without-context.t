This test checks whether a custom lock dir can be created, without having to
specify it in the context.

  $ mkrepo
  $ mkpkg a <<EOF
  > EOF
  $ mkpkg b <<EOF
  > EOF

  $ cat > dune-workspace <<EOF
  > (lang dune 3.12)
  > (lock_dir
  >  (path foo.lock)
  >  (repositories mock))
  > (repository
  >  (name mock)
  >  (url "file://$(pwd)/mock-opam-repository"))
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.12)
  > (package
  >  (name foo)
  >  (depends a b))
  > EOF

Specifying the directory to the lock command should work:

  $ dune_pkg_lock_normalized foo.lock
  Solution for foo.lock:
  - a.0.0.1
  - b.0.0.1
