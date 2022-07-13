Issue #4814:
When multiple packages are present, `dune build -p PKG` and `dune install -p
PKG` should be able to build and install only that package, instead of all.
  $ dune build -p pkg1
  $ dune install -p pkg1
  Error: The following <package>.install are missing:
  - _build/default/pkg2.install
  Hint: try running: dune build [-p <pkg>] @install
  [1]
