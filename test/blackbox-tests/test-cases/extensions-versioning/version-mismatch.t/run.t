Test that version of extensions is compatible with dune_lang version

  $ dune build
  File "dune-project", line 2, characters 14-17:
  2 | (using menhir 2.0)
                    ^^^
  Warning: Version 2.0 of the menhir extension is not supported until version
  1.4 of the dune language.
  Supported versions of this extension in version 1.2 of the dune language:
  - 1.0

