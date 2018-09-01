Variant feature is auto enabled when virtual_modules is used

  $ dune build --root variants-without-using
  Entering directory 'variants-without-using'

  $ dune build --root variants-using
  Entering directory 'variants-using'

virtual libraries may not implement their virtual modules

  $ dune build --root invalid-virtual-lib
  Entering directory 'invalid-virtual-lib'
  File "dune", line 3, characters 18-21:
  3 |  (virtual_modules foo bar))
                        ^^^
  Error: Module Foo has an implementation, it cannot be listed here
  [1]
