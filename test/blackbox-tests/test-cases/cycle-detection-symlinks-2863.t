Avoids false cycle detection when multiple symlinks point to the same tree.

  $ make_dune_project 2.0
  $ mkdir src
  $ ln -s src src-clone
  $ dune build
