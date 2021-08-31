Running dune utop in a project with only executables loads external libraries
  $ dune utop bin -- load_executable_libraries.ml
  File "load_executable_libraries.ml", line 2, characters 2-9:
  2 |   Bar.run ();
        ^^^^^^^
  Error: Unbound module Bar
  [2]
