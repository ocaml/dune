Exercise the `dune tools install ocamllsp` command.

  $ . ../helpers.sh
  $ . ./helpers.sh

  $ mkrepo
  $ make_mock_ocamllsp_package
  $ mkpkg ocaml 5.2.0

  $ setup_ocamllsp_workspace

  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > 
  > (package
  >  (name foo)
  >  (allow_empty))
  > EOF

  $ make_lockdir
  $ cat > dune.lock/ocaml.pkg <<EOF
  > (version 5.2.0)
  > EOF

Install ocamllsp but do not run it.
  $ dune tools install ocamllsp
  Solution for dev-tools.locks/ocaml-lsp-server:
  - ocaml.5.2.0
  - ocaml-lsp-server.0.0.1

Run ocamllsp (no solving or building necessary).
  $ dune tools exec ocamllsp
       Running 'ocamllsp'
  hello from fake ocamllsp
