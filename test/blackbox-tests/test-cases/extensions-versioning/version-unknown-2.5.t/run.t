TODO $ dune build Should raise an error and not a warning as in
version-mismatch.t

  $ dune build
  File "dune-project", line 2, characters 14-17:
  2 | (using menhir 4.0)
                    ^^^
  Error: Version 4.0 of the menhir extension is not supported.
  Supported versions of this extension in version 2.5 of the dune language:
  - 1.0 to 1.1
  - 2.0 to 2.1
  [1]

