Implementation of library from another project is allowed when explicitely
declared in the virtual library definition.

  $ dune build exe/exe.exe

  $ dune build -p vlibfoo-ext
  File "prj1/dune", line 6, characters 0-94:
  6 | (external_variant
  7 |   (virtual_library vlibfoo)
  8 |   (variant somevariant)
  9 |   (implementation impl))
  Error: Library "impl" not found.
  Hint: try: dune external-lib-deps --missing -p vlibfoo-ext @install
  [1]

  $ cat _build/install/default/lib/vlibfoo-ext/dune-package
  cat: _build/install/default/lib/vlibfoo-ext/dune-package: No such file or directory
  [1]
