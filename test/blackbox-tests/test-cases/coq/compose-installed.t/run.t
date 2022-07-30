We test composing a project with an installed Coq theory. The installed theory
does *not* have to be a dune project.

  $ export COQLIB=.

TODO: Currently this is not supported so the output is garbage

  $ cd A
  $ dune build
  File "dune", line 3, characters 11-12:
  3 |  (theories B))
                 ^
  Theory B not found
  -> required by theory A in .
  -> required by _build/default/a.v.d
  -> required by _build/default/a.glob
  -> required by alias all
  -> required by alias default
  [1]

