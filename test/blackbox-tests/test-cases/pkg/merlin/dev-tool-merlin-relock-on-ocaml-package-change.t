A dev tool that must use the project's compiler must be relocked when the
compiler package changes, even if its name and version stay the same.

  $ mkrepo
  $ make_mock_merlin_package
  $ mk_ocaml 5.2.0

  $ setup_merlin_workspace
  $ make_named_package_project foo 3.16 "(ocaml (= 5.2.0))"
  $ dune build

Create the initial dev-tool lock directory.

  $ dune tools exec ocamlmerlin >/dev/null 2>&1

An unchanged compiler package must keep the existing dev-tool lock directory.
Use an extra file to distinguish reuse from an identical regenerated lockdir.

  $ touch "${dev_tool_lock_dir}"/relock-sentinel
  $ dune tools exec ocamlmerlin >/dev/null 2>&1
  $ test -e "${dev_tool_lock_dir}"/relock-sentinel \
  > && echo 'unchanged dev-tool lock reused'
  unchanged dev-tool lock reused
  $ rm "${dev_tool_lock_dir}"/relock-sentinel

Change the compiler package's build recipe without changing its version, then
regenerate only the project lock directory.

  $ cat >>mock-opam-repository/packages/ocaml-base-compiler/ocaml-base-compiler.5.2.0/opam <<'EOF'
  > build: [ [ "echo" "compiler-recipe-v2" ] ]
  > EOF
  $ rm -rf dune.lock
  $ dune pkg lock >/dev/null 2>&1
  $ grep -q compiler-recipe-v2 dune.lock/ocaml-base-compiler*.pkg \
  > && echo 'project compiler updated'
  project compiler updated

Executing Merlin must notice the changed compiler package and regenerate its
lock directory from the updated recipe.

  $ dune tools exec ocamlmerlin >/dev/null 2>&1
  $ grep -q compiler-recipe-v2 "${dev_tool_lock_dir}"/ocaml-base-compiler*.pkg \
  > && echo 'dev-tool compiler updated'
  dev-tool compiler updated
