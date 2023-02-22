Reject flags in version [1.10, 3.8)
----------------------------------------------

  $ mkdir flags-unsupported
  $ cat > flags-unsupported/dune-project <<EOF
  > (lang dune 1.10)
  > (name foo)
  > (generate_opam_files true)
  > (package (name foo) (flags :plugin))
  > EOF

  $ dune build --root=flags-unsupported
  Entering directory 'flags-unsupported'
  File "dune-project", line 4, characters 20-35:
  4 | (package (name foo) (flags :plugin))
                          ^^^^^^^^^^^^^^^
  Error: 'flags' is only available since version 3.8 of the dune language.
  Please update your dune-project file to have (lang dune 3.8).
  Leaving directory 'flags-unsupported'
  [1]


Allow flags in >= 3.8
-----------------------------------------

  $ mkdir flags-supported
  $ cat > flags-supported/dune-project <<EOF
  > (lang dune 3.8)
  > (name foo)
  > (generate_opam_files true)
  > (license MIT ISC)
  > (package (name foo) (flags :plugin) (allow_empty))
  > EOF

  $ dune build --root=flags-supported
  Entering directory 'flags-supported'
  Leaving directory 'flags-supported'
  $ grep "flags:" flags-supported/foo.opam
  flags: plugin

Allow multiple flags
---------------------------------------

  $ mkdir multi-flags
  $ cat > multi-flags/dune-project <<EOF
  > (lang dune 3.8)
  > (name foo)
  > (generate_opam_files true)
  > (license ISC)
  > (package (name foo) (flags :plugin :compiler) (allow_empty))
  > EOF

  $ dune build --root=multi-flags
  Entering directory 'multi-flags'
  Leaving directory 'multi-flags'
  $ grep "flags:" multi-flags/foo.opam
  flags: [plugin compiler]

Reject empty flags
---------------------------------------

  $ mkdir empty-flags
  $ cat > empty-flags/dune-project <<EOF
  > (lang dune 3.8)
  > (name foo)
  > (generate_opam_files true)
  > (package (name foo) (flags) (allow_empty))
  > EOF

  $ dune build --root=empty-flags
  Entering directory 'empty-flags'
  File "dune-project", line 4, characters 20-27:
  4 | (package (name foo) (flags) (allow_empty))
                          ^^^^^^^
  Error: Not enough arguments for flags
  Leaving directory 'empty-flags'
  [1]
