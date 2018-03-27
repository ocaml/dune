  $ $JBUILDER runtest --root . -j 1 --display quiet
  File "jbuild", line 4, characters 20-42:
  Error: Library "ppx_that_doesn't_exist" not found.
  Hint: try: jbuilder external-lib-deps --missing --root . @runtest
  [1]

This should print something:

  $ $JBUILDER external-lib-deps --root . -j 1 --display quiet --missing @runtest
