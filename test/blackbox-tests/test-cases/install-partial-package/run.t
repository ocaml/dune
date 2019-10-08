we do not proceed with installation if some files in the .install
are missing.

  $ dune build @install
  $ rm -rf _build/install/default/bin
  $ dune install
  Error: The following files which are listed in _build/default/foo.install
  cannot be installed because they do not exist:
  - _build/install/default/bin/foo
  [1]
