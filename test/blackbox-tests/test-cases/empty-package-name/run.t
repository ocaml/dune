A package name can not be empty

  $ dune build
  File "dune", line 3, characters 14-25:
  3 |  (public_name .iaminvalid))
                    ^^^^^^^^^^^
  Error: ".iaminvalid" is an invalid library name.
  [1]
