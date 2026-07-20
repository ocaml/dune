Test that relocatable compilers bypass toolchain cache. A compiler is
considered relocatable if it's OCaml 5.5.0 or newer, or if the compiler
package's dependency closure contains an older relocatable compiler marker.

  $ mkrepo
  $ mkdir -p relocatable-repo/packages

  $ append_fake_compiler_commands() {
  > local opam="$1"
  > local contents="$2"
  > cat >> "$opam" <<EOF
  > build: [
  >   [ "sh" "-c" "echo '${contents}' > ocamlc.opt" ]
  >   [ "ln" "-s" "ocamlc.opt" "ocamlc" ]
  > ]
  > install: [
  >   [ "mkdir" "-p" "%{bin}%" ]
  >   [ "cp" "ocamlc.opt" "%{bin}%/" ]
  >   [ "cp" "-P" "ocamlc" "%{bin}%/" ]
  > ]
  > EOF
  > }

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
  > EOF
  $ append_fake_compiler_commands \
  >   relocatable-repo/packages/ocaml-base-compiler/ocaml-base-compiler.5.4.0/opam \
  >   "ocamlc binary"

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

With the shared cache disabled, building the compiler twice should reuse the
workspace-local target from the first build.

  $ rm -rf _build dune-cache
  $ export DUNE_CACHE=disabled
  $ build_pkg relocatable-compiler
  $ build_pkg relocatable-compiler 2>&1 | grep "Building relocatable-compiler"
  [1]
  $ dune trace cat | jq -r '
  >   select(.cat == "cache" and .name == "workspace_local_miss")
  >   | select(.args.head | test("relocatable-compiler.*target$"))
  >   | .args.reason
  > '

Test that OCaml 5.5 and newer compilers are treated as relocatable based on
their version.

We clean up the previous build state first:

  $ rm -rf _build dune.lock dune-cache mock-opam-repository
  $ export DUNE_CACHE=enabled
  $ mkrepo

  $ mkpkg ocaml-compiler 5.5.0 << 'EOF'
  > EOF
  $ append_fake_compiler_commands \
  >   "$mock_packages/ocaml-compiler/ocaml-compiler.5.5.0/opam" \
  >   "relocatable ocamlc"

  $ mkpkg ocaml-base-compiler 5.5.0 << 'EOF'
  > depends: [ "ocaml-compiler" {= "5.5.0"} ]
  > EOF

  $ mkpkg ocaml 5.5.0 << 'EOF'
  > depends: [ "ocaml-base-compiler" {= "5.5.0"} ]
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
  >  (repositories mock))
  > (repository
  >  (name mock)
  >  (url "file://$PWD/mock-opam-repository"))
  > EOF

Solving for OCaml 5.5 should pick the split compiler packages:

  $ dune_pkg_lock_normalized
  Solution for dune.lock:
  - ocaml.5.5.0
  - ocaml-base-compiler.5.5.0
  - ocaml-compiler.5.5.0

  $ build_pkg ocaml-compiler

Toolchain cache should be empty:

  $ test -d dune-cache/toolchains
  [1]

Check the installed files from the compiler are hardlinked (cached as regular
package):

  $ dune_cmd stat hardlinks $(get_build_pkg_dir ocaml-compiler)/target/bin/ocamlc.opt
  3
