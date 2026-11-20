Rocq language 0.15 requires Dune language 3.26.

  $ cat > dune-project.new << EOF
  > (lang dune 3.25)
  > (using rocq 0.15)
  > EOF
  $ mv dune-project.new dune-project
  $ dune build
  File "dune-project", line 2, characters 12-16:
  2 | (using rocq 0.15)
                  ^^^^
  Error: Version 0.15 of Rocq Prover build language is not supported until
  version 3.26 of the dune language.
  Supported versions of this extension in version 3.25 of the dune language:
  - 0.1 to 0.14
  [1]

public_name is gated on Rocq language 0.15
  $ cat > dune-project.new << EOF
  > (lang dune 3.24)
  > (using rocq 0.14)
  > EOF
  $ mv dune-project.new dune-project
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
  $ cat > dune-project.new << EOF
  > (lang dune 3.26)
  > (using rocq 0.15)
  > (package (name Foo))
  > EOF
  $ mv dune-project.new dune-project
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

Rocq language 0.15 requires a Rocq version with -package support.
  $ root=$PWD
  $ real_rocq=$(command -v rocq)
  $ mkdir old-bin old-rocq
  $ cat > old-bin/rocq << EOF
  > #!/bin/sh
  > if [ "\$1" = "--print-version" ]; then
  >   echo "9.2.0 fake-ocaml"
  > else
  >   exec "$real_rocq" "\$@"
  > fi
  > EOF
  $ chmod +x old-bin/rocq
  $ cd old-rocq
  $ make_rocq_project 3.26 0.15
  $ cat >> dune-project << EOF
  > (package (name foo))
  > EOF
  $ cat > dune << EOF
  > (rocq.theory
  >  (name Foo)
  >  (public_name foo))
  > EOF
  $ echo 'Definition x := 0.' > foo.v

  $ PATH="$root/old-bin:$PATH" dune build
  File "dune", lines 1-3, characters 0-44:
  1 | (rocq.theory
  2 |  (name Foo)
  3 |  (public_name foo))
  Error: Rocq language 0.15 requires Rocq 9.3 or newer, but version 9.2.0 was
  found.
  [1]
