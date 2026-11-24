A stale dev-tool lock directory can describe a different dependency closure for
the same compiler version as the project.

Use the shape of the relocatable compiler overlay: ocaml-base-compiler depends
on ocaml-compiler, which depends on relocatable-compiler. The latter performs
the compiler build and uses the overlay's package- and dependency-scoped
variables.

  $ mkrepo
  $ make_mock_merlin_package
  $ cat >>"$mock_packages/merlin/merlin.0.0.1/opam" <<'EOF'
  > depends: [ "ocaml" "tool-support" ]
  > EOF
  $ make_mock_dev_tool_package ocaml-lsp-server ocamllsp \
  >   "hello from fake ocamllsp"
  $ cat >>"$mock_packages/ocaml-lsp-server/ocaml-lsp-server.0.0.1/opam" <<'EOF'
  > depends: [ "ocaml" "tool-support" ]
  > EOF
  $ mkpkg tool-support <<'EOF'
  > EOF
  $ mkpkg project-only <<'EOF'
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
  $ make_named_package_project foo 3.16 "(ocaml (= 5.2.0))" project-only
  $ export DUNE_CACHE=disabled
  $ dune pkg lock >/dev/null 2>&1
  $ grep -q old-compiler-dependency \
  >   dune.lock/relocatable-compiler*.pkg
  $ dune build >project-output 2>&1
  $ dune tools exec ocamlmerlin >merlin-output 2>&1
  $ dune tools exec ocamllsp >lsp-output 2>&1
The three locks select the same compiler version, but each owns its compiler
target. Inspect the dependencies of the installed tools rather than counting
recipe executions: a cache hit is not a shared target.

  $ dependency_target() {
  >   exe=$(dune tools which "$1") || return 1
  >   target=$(dirname "$(dirname "$exe")")
  >   dune rules --format=json "$target" | jq_dune -er --arg pkg "$2" '
  >     [.[] | ruleDepFilePaths | select(test("/" + $pkg + "([.][^/]*)?/target$"))]
  >     | unique | if length == 1 then .[0] else error("expected one package target") end'
  > }
  $ project_only=$(get_build_pkg_dir project-only)/target
  $ for tool in ocamlmerlin ocamllsp; do
  >   exe=$(dune tools which "$tool") || exit 1
  >   dune rules --format=json "$(dirname "$(dirname "$exe")")" |
  >     jq_dune --arg target "$project_only" '.[] | ruleHasDepFile($target)'
  > done
  false
  false
  $ merlin_compiler=$(dependency_target ocamlmerlin relocatable-compiler)
  $ lsp_compiler=$(dependency_target ocamllsp relocatable-compiler)
  $ project_compiler=$(get_build_pkg_dir relocatable-compiler)/target
  $ test -d "$project_compiler"
  $ test "$merlin_compiler" != "$project_compiler"
  $ test "$lsp_compiler" != "$project_compiler"
  $ test "$merlin_compiler" != "$lsp_compiler"

An ordinary helper selected identically by both tools is private too.

  $ merlin_private=$(dependency_target ocamlmerlin tool-support)
  $ lsp_private=$(dependency_target ocamllsp tool-support)
  $ test "$merlin_private" != "$lsp_private"
  $ stale_target=$(dependency_target ocamllsp old-compiler-dependency)
  $ test -d "$stale_target"
  $ grep -q old-compiler-dependency \
  >   "$dev_tool_lock_dir"/relocatable-compiler*.pkg
  $ grep -q old-compiler-dependency \
  >   "$ocamllsp_lock_dir"/relocatable-compiler*.pkg

Keep both dev-tool lock directories stale while changing the relocatable
compiler's closure without changing the project compiler's name or version.
Regenerate the project lock and remove the stale dependency's installed target
so that a later attempt to build it is observable.

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
  $ rm -rf dune.lock "$pkg_root" "$stale_target"
  $ dune pkg lock >/dev/null 2>&1
  $ grep -q new-compiler-dependency \
  >   dune.lock/relocatable-compiler*.pkg
  $ dune build >/dev/null 2>&1

Delete and relock only Merlin. Ocamllsp's lock directory still contains the old
compiler, but Merlin must use the dependency from its new lock.

  $ rm -rf "$dev_tool_lock_dir"
  $ dune tools exec ocamlmerlin >merlin-output 2>&1
  $ grep -q new-compiler-dependency \
  >   "$dev_tool_lock_dir"/relocatable-compiler*.pkg
  $ grep -q old-compiler-dependency \
  >   "$ocamllsp_lock_dir"/relocatable-compiler*.pkg
  $ dependency_target ocamlmerlin new-compiler-dependency >/dev/null

The project package set does not include the stale tool's dependency.

  $ dune build @pkg-install >output 2>&1
  $ grep "building old compiler dependency" output
  [1]
  $ test ! -e "$stale_target"
