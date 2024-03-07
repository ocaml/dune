  $ . ./helpers.sh

  $ mkrepo

  $ mkpkg a <<EOF
  > EOF

  $ mkpkg b <<EOF
  > EOF

  $  cat > dune-workspace <<EOF
  > (lang dune 3.12)
  > (lock_dir
  >  (path foo.lock)
  >  (repositories mock))
  > (repository
  >  (name mock)
  >  (source "file://$(pwd)/mock-opam-repository"))
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.12)
  > (package
  >  (name foo)
  >  (depends a b))
  > EOF

  $ dune pkg lock foo.lock
  Solution for foo.lock:
  - a.0.0.1
  - b.0.0.1
