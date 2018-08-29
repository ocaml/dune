Variant feature is auto enabled when virtual_modules is used

  $ dune build --root variants-without-using
  File "dune", line 3, characters 1-25:
   (virtual_modules foobar))
   ^^^^^^^^^^^^^^^^^^^^^^^^
  Error: 'virtual_modules' is only available since version 0.1 of the experimental variants feature
  [1]

  $ dune build --root variants-using
  File "dune-project", line 2, characters 42-45:
  2: 
  Error: Version 0.1 of in_development_do_not_use_variants is not supported.
  Supported versions:
  - 0.0
  [1]
