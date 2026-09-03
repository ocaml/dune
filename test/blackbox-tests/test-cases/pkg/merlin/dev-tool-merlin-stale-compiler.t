A stale dev-tool lock directory can describe a different dependency closure for
the same compiler version as the project.

Use the shape of the relocatable compiler overlay: ocaml-base-compiler depends
on ocaml-compiler, which depends on relocatable-compiler. The latter performs
the compiler build and uses the overlay's package- and dependency-scoped
variables.

  $ mkrepo
  $ make_mock_merlin_package
  $ cat >>"$mock_packages/merlin/merlin.0.0.1/opam" <<'EOF'
  > depends: [ "ocaml" ]
  > EOF
  $ make_mock_dev_tool_package ocaml-lsp-server ocamllsp \
  >   "hello from fake ocamllsp"
  $ cat >>"$mock_packages/ocaml-lsp-server/ocaml-lsp-server.0.0.1/opam" <<'EOF'
  > depends: [ "ocaml" ]
  > EOF
  $ mk_ocaml 5.2.0
  $ mkpkg compiler-cloning enabled <<'EOF'
  > EOF
  $ cat >"$mock_packages/ocaml-compiler/ocaml-compiler.5.2.0/opam" <<'EOF'
  > opam-version: "2.0"
  > depends: [
  >   "ocaml" {= "5.2.0" & post}
  >   "relocatable-compiler"
  > ]
  > EOF
  $ mkpkg old-compiler-dependency <<'EOF'
  > build: [ [ "echo" "building old compiler dependency" ] ]
  > EOF
  $ mkpkg new-compiler-dependency <<'EOF'
  > EOF

Make the initial relocatable compiler depend on a package that will later be
removed from its dependency closure.

  $ compiler_pkg="$mock_packages/relocatable-compiler"
  $ compiler_pkg="$compiler_pkg/relocatable-compiler.5.2.0"
  $ mkdir -p "$compiler_pkg"
  $ cat >"$compiler_pkg/opam" <<'EOF'
  > opam-version: "2.0"
  > depends: [
  >   "ocaml" {= "5.2.0" & post}
  >   "compiler-cloning" {build}
  >   "old-compiler-dependency"
  > ]
  > build: [
  >   [ "echo" "build compiler" "%{_:build-id}%"
  >     "%{_:name}%" "%{compiler-cloning:version}%" ]
  > ]
  > EOF

  $ ocamllsp_lock_dir="_build/.dev-tools.locks/ocaml-lsp-server"
  $ cat >dune-workspace <<EOF
  > (lang dune 3.20)
  > (lock_dir
  >  (path "$dev_tool_lock_dir")
  >  (repositories mock))
  > (lock_dir
  >  (path "$ocamllsp_lock_dir")
  >  (repositories mock))
  > (lock_dir
  >  (repositories mock))
  > (repository
  >  (name mock)
  >  (url "file://$PWD/mock-opam-repository"))
  > EOF
  $ make_named_package_project foo 3.16 "(ocaml (= 5.2.0))"
  $ export DUNE_CACHE=disabled
  $ dune pkg lock >/dev/null 2>&1
  $ grep -q old-compiler-dependency \
  >   dune.lock/relocatable-compiler*.pkg
  $ dune build >/dev/null 2>&1
  $ dune tools exec ocamlmerlin >/dev/null 2>&1
  $ dune tools exec ocamllsp >/dev/null 2>&1
  $ grep -q old-compiler-dependency \
  >   "$dev_tool_lock_dir"/relocatable-compiler*.pkg
  $ grep -q old-compiler-dependency \
  >   "$ocamllsp_lock_dir"/relocatable-compiler*.pkg

Keep both dev-tool lock directories stale while changing the relocatable
compiler's closure without changing the project compiler's name or version.
Regenerate the explicit project lock and remove the package build tree so that
any attempt to build the stale dependency is observable.

  $ cat >"$compiler_pkg/opam" <<'EOF'
  > opam-version: "2.0"
  > depends: [
  >   "ocaml" {= "5.2.0" & post}
  >   "compiler-cloning" {build}
  >   "new-compiler-dependency"
  > ]
  > build: [
  >   [ "echo" "build compiler" "%{_:build-id}%"
  >     "%{_:name}%" "%{compiler-cloning:version}%" ]
  > ]
  > EOF
  $ rm -rf dune.lock "$pkg_root"
  $ dune pkg lock >/dev/null 2>&1
  $ grep -q new-compiler-dependency \
  >   dune.lock/relocatable-compiler*.pkg
  $ dune build >/dev/null 2>&1

Delete and relock only Merlin. Ocamllsp's lock directory still contains the old
compiler, but this does not prevent Merlin from running or cause the project
compiler to be rebuilt.

  $ rm -rf "$dev_tool_lock_dir"
  $ dune tools exec ocamlmerlin >merlin-output 2>&1
  $ grep -q new-compiler-dependency \
  >   "$dev_tool_lock_dir"/relocatable-compiler*.pkg
  $ grep -q old-compiler-dependency \
  >   "$ocamllsp_lock_dir"/relocatable-compiler*.pkg
  $ grep "build compiler" merlin-output
  [1]

Only the compiler closure reachable after substitution is included in
@pkg-install, so the stale dependency is not rebuilt.

  $ dune build @pkg-install >output 2>&1
  $ grep "building old compiler dependency" output
  [1]
