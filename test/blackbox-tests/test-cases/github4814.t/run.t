Issue #4814:
When multiple packages are present, `dune build -p PKG` and `dune install -p
PKG` should be able to build and install only that package, instead of all.
  $ dune build -p pkg1
  $ dune install pkg1
