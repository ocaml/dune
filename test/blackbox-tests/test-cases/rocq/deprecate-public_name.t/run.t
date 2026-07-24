public_name is gated on Rocq language 0.15
  $ chmod u+w dune-project
  $ cat > dune-project << EOF
  > (lang dune 3.24)
  > (using rocq 0.14)
  > EOF
  $ cat > dune << EOF
  > (rocq.theory
  >  (name foo)
  >  (public_name Foo))
  > EOF

  $ dune build
  File "dune", line 3, characters 1-18:
  3 |  (public_name Foo))
       ^^^^^^^^^^^^^^^^^
  Error: 'public_name' is only available since version 0.15 of Rocq Prover
  build language. Please update your dune-project file to have (using rocq
  0.15).
  [1]

package and public_name are accepted together for migration; public_name chooses
the findlib package.
  $ cat > dune-project << EOF
  > (lang dune 3.25)
  > (using rocq 0.15)
  > (package (name Foo))
  > EOF
  $ cat > dune << EOF
  > (rocq.theory
  >  (name foo)
  >  (public_name Foo)
  >  (package Foo))
  > EOF

  $ dune build @install

legacy_install is only meaningful for public_name theories.
  $ cat > dune << EOF
  > (rocq.theory
  >  (name foo)
  >  (legacy_install))
  > EOF

  $ dune build
  File "dune", line 3, characters 1-17:
  3 |  (legacy_install))
       ^^^^^^^^^^^^^^^^
  Error: legacy_install requires public_name
  [1]
