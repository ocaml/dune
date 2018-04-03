  $ jbuilder external-lib-deps @install
  These are the external library dependencies in the default context:
  - a
  - b
  - c

Reproduction case for #484. The error should point to src/jbuild

  $ jbuilder build @install
  File "src/jbuild", line 4, characters 16-17:
  Error: Library "a" not found.
  Hint: try: jbuilder external-lib-deps --missing @install
  [1]
