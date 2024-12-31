Tests for the `dune tools which` command.

The ocamlformat case:
  $ cat >dune-project <<EOF
  > (lang dune 3.11)
  > EOF
  $ dune tools which ocamlformat
  _build/_private/default/.dev-tool/ocamlformat/ocamlformat/target/bin/ocamlformat

The ocamllsp case:
  $ cat >dune-project <<EOF
  > (lang dune 3.11)
  > EOF
  $ dune tools which ocamllsp
  _build/_private/default/.dev-tool/ocaml-lsp-server/ocaml-lsp-server/target/bin/ocamllsp
