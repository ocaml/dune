Test that version of extensions is compatible with dune_lang version
  $ dune build --root version-mismatch
  Entering directory 'version-mismatch'
  File "dune-project", line 2, characters 14-17:
  2 | (using menhir 2.0)
                    ^^^
  Error: Version 2.0 of menhir is not supported.
  Supported versions for dune lang 1.2:
  - 1.0
  [1]

Using fields in dune-project should be generated according to
the maximum supported version for the chosen dune lang version
  $ dune build --root using-generation
  Entering directory 'using-generation'
  Info: Appending this line to dune-project: (using menhir 1.0)

