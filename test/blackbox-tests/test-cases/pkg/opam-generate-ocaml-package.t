The ocaml compiler needs to be marked inside the lock dir:

  $ . ./helpers.sh
  $ mkrepo

To mark it, we use `conflict-class: "ocaml-core-compiler"`

  $ mkpkg foocaml <<EOF
  > conflict-class: "ocaml-core-compiler"
  > EOF

  $ solve foocaml
  Solution for .dune-solution-cache:
  - foocaml.0.0.1

  $ grep ocaml ${default_lock_dir}/lock.dune
  (ocaml foocaml)
