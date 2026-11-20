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

The compiler is installed outside the workspace:

  $ build_pkg ocaml-base-compiler
  $ find "$DUNE_CACHE_ROOT/toolchains" -type f -name ocamlc -exec cat {} \;
  mock ocamlc
  $ test -f "$(get_build_pkg_dir ocaml-base-compiler)/target/bin/ocamlc"
  [1]

The dependent package looks for the compiler inside the workspace instead:

  $ build_pkg consumer > consumer.log 2>&1
  [1]
  $ grep -o 'target/bin/ocamlc' consumer.log
  target/bin/ocamlc
