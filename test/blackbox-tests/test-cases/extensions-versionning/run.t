Test that version of extensions is compatible with dune_lang version

  $ cat >using-generation/dune-project <<EOF
  > (lang dune 1.2)
  > EOF

  $ dune build --root version-mismatch
  Entering directory 'version-mismatch'
  File "dune-project", line 2, characters 14-17:
  2 | (using menhir 2.0)
                    ^^^
  Warning: Version 2.0 of the menhir extension is not supported until version
  1.4 of the dune language.
  Supported versions of this extension in version 1.2 of the dune language:
  - 1.0

  $ dune build --root version-unknown
  Entering directory 'version-unknown'
  File "dune-project", line 2, characters 14-17:
  2 | (using menhir 4.0)
                    ^^^
  Error: Version 4.0 of the menhir extension is not supported.
  Supported versions of this extension in version 1.2 of the dune language:
  - 1.0
  [1]

  $ dune build --root version-unknown-2.5
  Entering directory 'version-unknown-2.5'
  File "dune-project", line 2, characters 14-17:
  2 | (using menhir 4.0)
                    ^^^
  Error: Version 4.0 of the menhir extension is not supported.
  Supported versions of this extension in version 2.5 of the dune language:
  - 1.0 to 1.1
  - 2.0 to 2.1
  [1]

TODO $ dune build --root version-mismatch-2.5 Should raise an error and
not a warning as in $ dune build --root version-mismatch

Using fields in dune-project should be generated according to
the maximum supported version for the chosen dune lang version
  $ dune build --root using-generation
  Entering directory 'using-generation'
  Info: Appending this line to dune-project: (using menhir 1.0)
