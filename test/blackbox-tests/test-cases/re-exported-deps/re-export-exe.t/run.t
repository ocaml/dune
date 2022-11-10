Re-exporting deps in executables isn't allowed
  $ dune build @all
  File "dune", line 7, characters 12-27:
  7 |  (libraries (re_export foo)))
                  ^^^^^^^^^^^^^^^
  Error: re_export is not allowed here
  [1]
