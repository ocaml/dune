Test the special compiler version is picked up by ocamllsp.

  $ . ../helpers.sh
  $ . ./helpers.sh

  $ mkrepo
  $ make_mock_ocamllsp_package
  $ mk_ocaml 5.2.0
  $ mkpkg ocaml-variants 5.2.0+ox << EOF
  > flags: compiler
  > conflict-class: "ocaml-core-compiler"
  > EOF

  $ setup_ocamllsp_workspace

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > 
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends
  >    (ocaml (= 5.2.0))
  >    (ocaml-variants (= 5.2.0+ox))))
  > EOF

  $ dune build

Here `ocamllsp` will pickup the compiler dependency on 5.2.0+ox
  $ dune tools exec ocamllsp
  Solution for _build/.dev-tools.locks/ocaml-lsp-server:
  - ocaml-lsp-server.0.0.1
  - ocaml-variants.5.2.0+ox
       Running 'ocamllsp'
  hello from fake ocamllsp

