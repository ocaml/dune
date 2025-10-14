Test `dune tools which ocamlformat`:
  $ . ./helpers.sh
  $ mkrepo

  $ make_fake_ocamlformat "0.26.2"
  $ make_ocamlformat_opam_pkg "0.26.2"
  $ make_project_with_dev_tool_lockdir

Update ".ocamlformat" file with unknown version of OCamlFormat.
  $ cat > .ocamlformat <<EOF
  > version = 0.26.2
  > EOF

The command will fail because the dev tool is not installed:
  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune tools which ocamlformat
  Error: ocamlformat is not installed as a dev tool
  [1]

  $ dune tools which ocamlformat --allow-not-installed
  _build/_private/default/.dev-tool/ocamlformat/target/bin/ocamlformat

Install the dev tool:
  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune tools exec ocamlformat
  Solution for dev-tools.locks/ocamlformat:
  - ocamlformat.0.26.2
       Running 'ocamlformat'
  formatted with version 0.26.2

Now the command will succeed because the tool has been installed:
  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune tools which ocamlformat
  _build/_private/default/.dev-tool/ocamlformat/target/bin/ocamlformat

Make sure the file is actually there:
  $ test -f $(dune tools which ocamlformat)
