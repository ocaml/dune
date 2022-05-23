public libraries may not have private dependencies

  $ dune build
  File "dune", line 8, characters 12-22:
  8 |  (libraries privatelib)
                  ^^^^^^^^^^
  Error: Library "privatelib" is private, it cannot be a dependency of a public
  library. You need to give "privatelib" a public name.
  [1]
