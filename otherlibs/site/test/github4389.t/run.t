  $ dune build @install
  $ dune install --prefix _install
  Installing _install/lib/github4389/META
  Installing _install/lib/github4389/dune-package
  File "_build/install/default/lib/github4389/dune-package", line 1, characters 0-0:
  Warning: Failed to parse file, not adding version and locations information.
  Installing _install/bin/main
  $ grep sites _install/lib/github4389/dune-package
  (sites (github4389 share))
  $ grep -o '[^ ]*/_install/share/github4389' _install/lib/github4389/dune-package
  [1]
  $ _install/bin/main
  n/a
  $TESTCASE_ROOT/_install/share/github4389/github4389
