Test that relocatable compilers bypass toolchain cache. This includes:
- OCaml < 5.5 with the relocatable-compiler package
- OCaml >= 5.5 which is natively relocatable

This mirrors the package layout from
https://github.com/dra27/opam-repository/tree/relocatable

The relocatable repo is separate from the main opam repo and should take priority.

  $ mkrepo
  $ mkdir -p relocatable-repo/packages

Create the relocatable compiler packages in separate repo:

  $ mkdir -p relocatable-repo/packages/compiler-cloning/compiler-cloning.0.0.1
  $ cat > relocatable-repo/packages/compiler-cloning/compiler-cloning.0.0.1/opam << 'EOF'
  > opam-version: "2.0"
  > EOF

  $ mkdir -p relocatable-repo/packages/relocatable-compiler/relocatable-compiler.5.4.0
  $ cat > relocatable-repo/packages/relocatable-compiler/relocatable-compiler.5.4.0/opam << 'EOF'
  > opam-version: "2.0"
  > depends: [ "compiler-cloning" ]
  > build: [ "sh" "-c" "echo %{_:build-id}% > build-id.txt" ]
  > install: [ "cp" "build-id.txt" "%{lib}%/relocatable-compiler/" ]
  > EOF

  $ mkdir -p relocatable-repo/packages/ocaml-base-compiler/ocaml-base-compiler.5.4.0
  $ cat > relocatable-repo/packages/ocaml-base-compiler/ocaml-base-compiler.5.4.0/opam << 'EOF'
  > opam-version: "2.0"
  > depends: [ "relocatable-compiler" {= "5.4.0"} ]
  > build: [
  >   [ "sh" "-c" "echo 'ocamlc binary' > ocamlc.opt" ]
  >   [ "ln" "-s" "ocamlc.opt" "ocamlc" ]
  > ]
  > install: [
  >   [ "mkdir" "-p" "%{bin}%" ]
  >   [ "cp" "ocamlc.opt" "%{bin}%/" ]
  >   [ "cp" "-P" "ocamlc" "%{bin}%/" ]
  > ]
  > EOF

  $ mkdir -p relocatable-repo/packages/ocaml/ocaml.5.4.0
  $ cat > relocatable-repo/packages/ocaml/ocaml.5.4.0/opam << 'EOF'
  > opam-version: "2.0"
  > depends: [ "ocaml-base-compiler" {= "5.4.0"} ]
  > EOF

Also create a standard ocaml-base-compiler 5.4 in mock repo. This tests that
the relocatable repo takes priority:

  $ mkpkg ocaml-base-compiler 5.4.0 

  $ mkpkg ocaml 5.4.0 << 'EOF'
  > depends: [ "ocaml-base-compiler" {= "5.4.0"} ]
  > EOF

  $ cat > dune-project << 'EOF'
  > (lang dune 3.22)
  > (package
  >  (name test)
  >  (allow_empty)
  >  (depends ocaml))
  > EOF

  $ cat > dune-workspace << EOF
  > (lang dune 3.20)
  > (lock_dir
  >  (repositories relocatable-repo mock)) ; Order here is important here
  > (repository
  >  (name relocatable-repo)
  >  (url "file://$PWD/relocatable-repo"))
  > (repository
  >  (name mock)
  >  (url "file://$PWD/mock-opam-repository"))
  > EOF

We enable caching to verify that packages are built normally and get hardlinked
into the cache. A hardlink count > 1 proves the file was cached.

  $ export DUNE_CACHE=enabled
  $ export DUNE_CACHE_ROOT=$PWD/dune-cache

Solving for OCaml 5.4 should pick up the relocatable compiler.

  $ dune_pkg_lock_normalized
  Solution for dune.lock:
  - compiler-cloning.0.0.1
  - ocaml.5.4.0
  - ocaml-base-compiler.5.4.0
  - relocatable-compiler.5.4.0

  $ build_pkg ocaml-base-compiler

Toolchain cache should be empty:

  $ test -d dune-cache/toolchains
  [1]

Check the installed files from the compiler are hardlinked (cached as regular
package):

  $ dune_cmd stat hardlinks $(get_build_pkg_dir ocaml-base-compiler)/target/bin/ocamlc.opt
  3

For OCaml >= 5.5, relocatable is built-in, so no special packages needed. These
go in the main opam repo.

  $ mkpkg ocaml-base-compiler 5.5.0 << 'EOF'
  > flags: compiler
  > build: [ "sh" "-c" "echo 'native relocatable' > marker.txt" ]
  > install: [
  >   [ "mkdir" "-p" "%{lib}%/ocaml" ]
  >   [ "cp" "marker.txt" "%{lib}%/ocaml/" ]
  > ]
  > EOF

  $ mkpkg ocaml 5.5.0 << 'EOF'
  > depends: [ "ocaml-base-compiler" {= "5.5.0"} ]
  > EOF

  $ cat > dune-project << 'EOF'
  > (lang dune 3.16)
  > (package
  >  (name test)
  >  (allow_empty)
  >  (depends (ocaml (= 5.5.0))))
  > EOF

The relocatable repo will not have OCaml >= 5.5 so we do not need to bother
updating the repositories in the workspace.

  $ dune_pkg_lock_normalized
  Solution for dune.lock:
  - ocaml.5.5.0
  - ocaml-base-compiler.5.5.0

  $ build_pkg ocaml-base-compiler

Toolchain cache should be empty:

  $ test -d dune-cache/toolchains
  [1]

Check the installed files from the compiler are hardlinked (cached as regular
package):

  $ dune_cmd stat hardlinks $(get_build_pkg_dir ocaml-base-compiler)/target/lib/ocaml/marker.txt
  2
