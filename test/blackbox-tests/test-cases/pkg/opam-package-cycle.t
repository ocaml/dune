Testing how the solver handles cycles in an opam repository.

  $ . ./helpers.sh
  $ mkrepo

  $ mkpkg a <<'EOF'
  > depends: [ "b" ]
  > EOF
  $ mkpkg b <<'EOF'
  > depends: [ "c" ]
  > EOF
  $ mkpkg c <<'EOF'
  > depends: [ "a" ]
  > EOF

Solver doesn't complain about cycles.

  $ solve a
  Solution for dune.lock:
  - a.0.0.1
  - b.0.0.1
  - c.0.0.1
  $ solve b
  Solution for dune.lock:
  - a.0.0.1
  - b.0.0.1
  - c.0.0.1
  $ solve c
  Solution for dune.lock:
  - a.0.0.1
  - b.0.0.1
  - c.0.0.1
