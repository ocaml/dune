  $ $JBUILDER external-lib-deps --root . -j1 --display quiet @install
  These are the external library dependencies in the default context:
  - a
  - b
  - c

Reproduction case for #484. The error should point to src/jbuild

  $ $JBUILDER build --root . -j1 --display quiet @install
  Error: External library "a" not found.
  -> required by test/jbuild
  Hint: try: jbuilder external-lib-deps --missing --root . @install
  [1]
