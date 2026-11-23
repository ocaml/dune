Check where a compiler without a relocatability marker is installed and
whether its installed files are available to dependent packages.

  $ make_lockdir
  $ make_package_project 3.24 consumer
  $ export DUNE_CACHE_ROOT="$PWD/cache"

  $ make_lockpkg ocaml-base-compiler <<'EOF'
  > (version 5.4.0)
  > (build
  >  (run sh -ec "echo \"$1\" > prefix
  > echo 'mock ocamlc' > ocamlc" sh %{prefix}))
  > (install
  >  (run sh -ec "prefix=$(cat prefix)
  > mkdir -p \"$prefix/bin\"
  > cp ocamlc \"$prefix/bin\""))
  > EOF

  $ make_lockpkg consumer <<'EOF'
  > (version 1)
  > (depends ocaml-base-compiler)
  > (install
  >  (run cp %{pkg:ocaml-base-compiler:bin}/ocamlc %{share}/consumer/compiler))
  > EOF

The compiler is installed in the workspace, without using a separate cache:

  $ build_pkg ocaml-base-compiler
  $ cat "$(get_build_pkg_dir ocaml-base-compiler)/target/bin/ocamlc"
  mock ocamlc
  $ test ! -e "$DUNE_CACHE_ROOT/toolchains"

The dependent package can read the installed compiler:

  $ build_pkg consumer
  $ cat "$(get_build_pkg_dir consumer)/target/share/consumer/compiler"
  mock ocamlc
