`(and)`/`(or)` with no argument should get rejected.

  $ cat > dune-project << EOF
  > (lang dune 3.9)
  > (generate_opam_files)
  > (package
  >  (name p)
  >  (depends
  >   (p (and))))
  > EOF

  $ dune build
  File "dune-project", line 6, characters 5-10:
  6 |   (p (and))))
           ^^^^^
  Error: Logical operators with no arguments was deleted in version 3.9 of the
  dune language.
  [1]

  $ cat > dune-project << EOF
  > (lang dune 3.9)
  > (generate_opam_files)
  > (package
  >  (name p)
  >  (depends
  >   (p (or))))
  > EOF

  $ dune build
  File "dune-project", line 6, characters 5-9:
  6 |   (p (or))))
           ^^^^
  Error: Logical operators with no arguments was deleted in version 3.9 of the
  dune language.
  [1]

In < 3.9 they are accepted, assuming they are not used to generate opam files.

  $ cat > dune-project << EOF
  > (lang dune 3.8)
  > (package
  >  (name p)
  >  (allow_empty)
  >  (depends
  >   (p (and))))
  > EOF

  $ dune build

  $ cat > dune-project << EOF
  > (lang dune 3.8)
  > (package
  >  (name p)
  >  (allow_empty)
  >  (depends
  >   (p (or))))
  > EOF

  $ dune build

Generating opam files should trigger a (nice) error:

  $ cat > dune-project << EOF
  > (lang dune 3.8)
  > (generate_opam_files)
  > (package
  >  (name p)
  >  (depends
  >   (p (and))))
  > EOF

  $ dune build
  Error: logical operations with no arguments are not supported
  -> required by _build/default/p.opam
  -> required by _build/install/default/lib/p/opam
  -> required by _build/default/p.install
  -> required by alias all
  -> required by alias default
  [1]

  $ cat > dune-project << EOF
  > (lang dune 3.8)
  > (generate_opam_files)
  > (package
  >  (name p)
  >  (depends
  >   (p (or))))
  > EOF

  $ dune build
  Error: logical operations with no arguments are not supported
  -> required by _build/default/p.opam
  -> required by _build/install/default/lib/p/opam
  -> required by _build/default/p.install
  -> required by alias all
  -> required by alias default
  [1]
