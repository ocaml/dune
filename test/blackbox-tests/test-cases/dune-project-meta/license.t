Reject multiple licences in version [1.9, 3.2)
----------------------------------------------

  $ mkdir multi-licence-v1.9
  $ cat > dune-project <<EOF
  > (lang dune 1.9)
  > (name foo)
  > (generate_opam_files true)
  > (license MIT ISC)
  > (package (name foo) (allow_empty))
  > EOF

  $ dune build
  File "dune-project", line 4, characters 0-17:
  4 | (license MIT ISC)
      ^^^^^^^^^^^^^^^^^
  Error: Parsing several licenses is only available since version 3.2 of the
  dune language. Please update your dune-project file to have (lang dune 3.2).
  [1]

Allow multiple licences in version >= 3.2
-----------------------------------------

  $ mkdir multi-license-v3.2
  $ cat > dune-project <<EOF
  > (lang dune 3.2)
  > (name foo)
  > (generate_opam_files true)
  > (license MIT ISC)
  > (package (name foo) (allow_empty))
  > EOF

  $ dune build
  $ grep "license:" foo.opam
  license: ["MIT" "ISC"]

Handle single license in version >= 3.2
---------------------------------------

  $ mkdir single-license-v3.2
  $ cat > dune-project <<EOF
  > (lang dune 3.2)
  > (name foo)
  > (generate_opam_files true)
  > (license ISC)
  > (package (name foo) (allow_empty))
  > EOF

  $ dune build
  $ grep "license:" foo.opam
  license: "ISC"

Reject empty license
---------------------------------------
 
  $ cat > dune-project <<EOF
  > (lang dune 3.2)
  > (name foo)
  > (generate_opam_files true)
  > (license)
  > (package (name foo) (allow_empty))
  > EOF

  $ dune build
  File "dune-project", line 4, characters 0-9:
  4 | (license)
      ^^^^^^^^^
  Error: Not enough arguments for license
  [1]
