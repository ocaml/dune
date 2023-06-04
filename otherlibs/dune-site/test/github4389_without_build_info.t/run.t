  $ dune build @install
  $ dune install --prefix _install --display short
  Installing _install/lib/github4389/META
  Installing _install/lib/github4389/dune-package
  Installing _install/bin/main
  $ grep sites _install/lib/github4389/dune-package
  (sites (github4389 share))
  $ grep -o '[^ ]*/_install/share/github4389' _install/lib/github4389/dune-package
  $TESTCASE_ROOT/_install/share/github4389
  $ _install/bin/main
  $TESTCASE_ROOT/_install/share/github4389/github4389
