Issue #4814:
When multiple packages are present, `dune build -p PKG` and `dune install -p
PKG` should be able to build and install only that package, instead of all.

  $ dune build -p pkg1

This is now fixed, although we should really just not allow -p on $ dune
install

  $ dune install -p pkg1 --dry-run --prefix _install
