  $ . ./helpers.sh

Test for "dune tools install" command which installs all the available dev-tools .
The command locks and builds those dev-tools.

  $ mkrepo
  $ make_mock_tool_package odoc odoc
  $ make_mock_tool_package ocaml-lsp-server ocamllsp
  $ make_mock_tool_package ocamlformat ocamlformat
  $ mkpkg ocaml 5.2.0

Each dev-tool has it own lock-dir
  $ cat > dune-workspace <<EOF
  > (lang dune 3.16)
  > (lock_dir
  >   (path "dev-tools.locks/odoc")
  >   (repositories mock))
  > (lock_dir
  >   (path "dev-tools.locks/ocamlformat")
  >   (repositories mock))
  > (lock_dir
  >   (path "dev-tools.locks/ocaml-lsp-server")
  >   (repositories mock))
  > (lock_dir
  >   (repositories mock))
  > (repository
  >   (name mock)
  >   (source "file://$(pwd)/mock-opam-repository"))
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > (package
  >  (name foo)
  >  (depends ocaml))
  > EOF

Building dev-tool requires the project been locked.
  $ dune pkg lock
  Solution for dune.lock:
  - ocaml.5.2.0

  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune tools install
  Solution for dev-tools.locks/ocamlformat:
  - ocamlformat.0.0.1
  Solution for dev-tools.locks/odoc:
  - ocaml.5.2.0
  - odoc.0.0.1
  Solution for dev-tools.locks/ocaml-lsp-server:
  - ocaml.5.2.0
  - ocaml-lsp-server.0.0.1

Check, if the dev-tools are built.
  $ ls ./_build/_private/default/.dev-tool
  ocaml-lsp-server
  ocamlformat
  odoc
