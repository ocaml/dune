  $ jbuilder runtest --root . -j 1 --display quiet
  File "jbuild", line 4, characters 20-42:
  Error: Library "ppx_that_doesn't_exist" not found.
  Hint: try: jbuilder external-lib-deps --missing --root . @runtest
  [1]

These should print something:

  $ jbuilder external-lib-deps --root . -j 1 --display quiet @runtest

  $ jbuilder external-lib-deps --root . -j 1 --display quiet --missing @runtest
