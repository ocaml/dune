The ocaml compiler needs to be marked inside the lock dir:

  $ . ./helpers.sh
  $ mkrepo

To mark it, we use flags: compiler:

  $ mkpkg foocaml <<EOF
  > flags: compiler
  > EOF

  $ solve foocaml
  Solution for dune.lock:
  - foocaml.0.0.1

  $ grep ocaml dune.lock/lock.dune
  (ocaml foocaml)
