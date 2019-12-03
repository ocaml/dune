Test various errors related to resolving libraries

Cycle detection
---------------
  $ cat >dune <<EOF
  > (library
  >  (name a)
  >  (libraries b)
  >  (modules))
  > (library
  >  (name b)
  >  (libraries c)
  >  (modules))
  > (library
  >  (name c)
  >  (libraries a)
  >  (modules))
  > (executable
  >  (name cycle)
  >  (libraries a)
  >  (modules cycle))
  > EOF

  $ touch cycle.ml
  $ dune build cycle.exe
  Error: Dependency cycle detected between the following libraries:
     "a" in _build/default
  -> "b" in _build/default
  -> "c" in _build/default
  -> "a" in _build/default
  -> required by library "c" in _build/default
  -> required by executable cycle in dune:14
  [1]

Select with no solution
-----------------------
  $ cat >dune <<EOF
  > (executable
  >  (name select_error)
  >  (libraries (select x from))
  >  (modules select_error))
  > EOF
  $ touch select_error.ml
  $ dune build select_error.exe
  File "dune", line 3, characters 12-27:
  3 |  (libraries (select x from))
                  ^^^^^^^^^^^^^^^
  Error: No solution found for this select form.
  [1]

