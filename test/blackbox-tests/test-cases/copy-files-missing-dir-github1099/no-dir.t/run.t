If the source directory does not exist, an error message is printed:

  $ dune build demo.exe
  File "dune", line 1, characters 13-23:
  1 | (copy_files# "no_dir/*")
                   ^^^^^^^^^^
  Error: Cannot find directory: no_dir
  [1]
