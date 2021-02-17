Test that version of extensions is compatible with dune_lang version

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

Check the error message when using an extension that is not available
at the current language version:
  $ dune build --root using-and-dune-lang
  Entering directory 'using-and-dune-lang'
  File "dune", line 1, characters 0-5:
  1 | (mdx)
      ^^^^^
  Error: 'mdx' is available only when mdx is enabled in the dune-project file.
  You must enable it using (using mdx ..) in your dune-project file.
  Note however that the currently selected version of dune (1.0) does not
  support this plugin. The first version of this plugin is 0.1 and was
  introduced in dune 2.4.
  [1]
