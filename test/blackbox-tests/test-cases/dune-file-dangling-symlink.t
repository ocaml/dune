A dune file that is a dangling symbolic link is reported as an error.

  $ make_dune_project 3.25
  $ mkdir sub && ln -s missing sub/dune
  $ dune build
  File "sub/dune", line 1, characters 0-0:
  Error: File sub/dune cannot be read.
  [1]
