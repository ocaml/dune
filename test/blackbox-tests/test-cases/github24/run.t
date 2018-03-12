  $ $JBUILDER build -j1 @install --display short --root . --debug-dependency-path
  File "jbuild", line 2, characters 1-94:
  Error: Library "x" in _build/default is hidden (optional with unavailable dependencies).
  Hint: try: jbuilder external-lib-deps --missing --root . @install
  [1]
