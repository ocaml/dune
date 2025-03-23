Reproducing github #11527

When running dune exec on a binary `bug` we can expect that the working directory and
argv.(0) can be concatenated to get a valid path.

When running dune exec from the root this is true.
  $ dune exec -- bug
  pwd: $TESTCASE_ROOT
  exe: _build/install/default/bin/bug

However when running dune exec from a subdirectory, arg.(0) is not a valid path from the
working directory.
  $ (cd subdir && dune exec --root .. -- bug)
  Entering directory '..'
  Leaving directory '..'
  pwd: $TESTCASE_ROOT/subdir
  exe: _build/install/default/bin/bug
 
