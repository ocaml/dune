  $ jbuilder runtest
  File "jbuild", line 4, characters 20-42:
  Error: Library "ppx_that_doesn't_exist" not found.
  Hint: try: jbuilder external-lib-deps --missing @runtest
  [1]

These should print something:

  $ jbuilder external-lib-deps @runtest

  $ jbuilder external-lib-deps --missing @runtest
