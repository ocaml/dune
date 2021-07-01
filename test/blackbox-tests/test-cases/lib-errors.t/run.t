Test various errors related to resolving libraries

Cycle detection
---------------

  $ dune build cycle.exe
  Error: Dependency cycle between:
     library "a" in _build/default
  -> library "c" in _build/default
  -> library "b" in _build/default
  -> library "a" in _build/default
  [1]

Select with no solution
-----------------------

  $ mkdir select
  $ cd select
  $ echo "(lang dune 3.0)" > dune-project
  $ cat <<EOF > dune
  > (executable
  >  (name select_error)
  >  (libraries (select x from))
  >  (modules select_error))
  > (rule (with-stdout-to select_error.ml (echo "")))
  > EOF

  $ dune build select_error.exe
  File "dune", line 3, characters 12-27:
  3 |  (libraries (select x from))
                  ^^^^^^^^^^^^^^^
  Error: No solution found for this select form.
  [1]

