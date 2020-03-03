virtual libraries may not implement their virtual modules

  $ dune build
  File "dune", line 3, characters 18-21:
  3 |  (virtual_modules foo bar))
                        ^^^
  Error: The following modules have an implementation, they cannot be listed as
  virtual:
  - Foo
  [1]
