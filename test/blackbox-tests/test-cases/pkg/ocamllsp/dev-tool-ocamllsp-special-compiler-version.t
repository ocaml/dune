Test the special compiler version is picked up by ocamllsp.

  $ . ../helpers.sh
  $ . ./helpers.sh

  $ mkrepo
  $ make_mock_ocamllsp_package
  $ mkpkg ocaml-variants 5.2.0+ox
  $ mkpkg ocaml 5.2.0

  $ setup_ocamllsp_workspace

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > 
  > (package
  >  (name foo)
  >  (allow_empty))
  > EOF

  $ make_lockdir
  $ make_lockpkg ocaml-variants <<EOF
  > (version 5.2.0+ox)
  > EOF
  $ make_lockpkg ocaml <<EOF
  > (version 5.2.0)
  > EOF

  $ dune tools exec ocamllsp
  Solution for _build/.dev-tools.locks/ocaml-lsp-server:
  - ocaml.5.2.0
  - ocaml-lsp-server.0.0.1
  - ocaml-variants.5.2.0+ox
       Running 'ocamllsp'
  hello from fake ocamllsp

