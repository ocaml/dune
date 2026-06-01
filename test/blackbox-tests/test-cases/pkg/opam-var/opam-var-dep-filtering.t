Demonstrate how dependencies are filtered in opam files:

  $ mkrepo

  $ mkpkg "foo"

Regular dependencies

  $ mkpkg "testpkg" 1 <<'EOF' 
  > depends: [ "foo" {version = 1} ]
  > EOF

  $ solve testpkg
  Solution for dune.lock:
  - foo.0.0.1
  - testpkg.1

  $ mkpkg "testpkg" 2 <<'EOF' 
  > depends: [ "foo" {version = 1} ]
  > EOF

  $ solve testpkg
  Solution for dune.lock:
  - testpkg.2

Depopts. We don't have proper support for depopts yet, so these don't work.
When depopts are enabled though, the test should demonstrate that depopts works
the same way as depends

  $ mkpkg "testpkg" 1 <<'EOF' 
  > depopts: [ "foo" {version = 1} ]
  > EOF

  $ solve testpkg
  Solution for dune.lock:
  - testpkg.2

  $ mkpkg "testpkg" 2 <<'EOF' 
  > depopts: [ "foo" {version = 1} ]
  > EOF

  $ solve testpkg
  Solution for dune.lock:
  - testpkg.2
