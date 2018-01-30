  $ $JBUILDER build -j1 --root . @install
  Error: External library "a_kernel" not found.
  -> required by "required by (pps (a_kernel))"
  Hint: try: jbuilder external-lib-deps --missing --root . @install
  [1]
