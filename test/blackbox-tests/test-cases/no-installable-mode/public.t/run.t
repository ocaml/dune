When a public executable is built in shared_object mode, a specific error
message is displayed:

  $ dune build --display=short
  File "dune", line 1, characters 0-70:
  1 | (executable
  2 |  (name mylib)
  3 |  (public_name mylib)
  4 |  (modes shared_object))
  Error: No installable mode found for this executable.
  When public_name is set, one of the following modes is required:
  - exe
  - native
  - byte
  [1]
