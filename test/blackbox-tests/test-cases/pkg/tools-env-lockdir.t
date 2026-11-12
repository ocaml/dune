[dune tools env] should also put the binaries installed by packages dependencies in PATH.

  $ mkrepo
  $ mkpkg provider <<'EOF'
  > build: [
  >   ["sh" "-c" "echo '#!/bin/sh' > helper"]
  >   ["sh" "-c" "echo 'echo from helper' >> helper"]
  >   ["chmod" "+x" "helper"]
  >   ["sh" "-c" "echo 'bin: [ \"helper\" ]' > provider.install"]
  > ]
  > EOF

  $ solve provider
  Solution for dune.lock:
  - provider.0.0.1

Build the package so that its binary is installed:

  $ build_pkg provider

  $ (eval $(dune tools env) && helper)
  from helper

  $ dune tools env | grep -c provider
  1

Once the dependency is removed and the project relocked, the package's binary
directory is no longer added to PATH:

  $ solve
  Solution for dune.lock:
  (no dependencies to lock)

  $ dune tools env | grep -c provider
  0
  [1]
