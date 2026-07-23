dune install should not write anything to _build/
  $ make_dune_project 2.8
  $ dune install --prefix _install
  $ ls .
  dune-project
