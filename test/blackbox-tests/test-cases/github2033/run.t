. should be allowed in c names
  $ dune build
  Info: creating file dune-project with this contents:
  | (lang dune 1.9)
  
  File "dune", line 3, characters 10-17:
  3 |  (c_names file.xx))
                ^^^^^^^
  Error: file.xx does not exist as a C source. file.xx.c must be present
  [1]
