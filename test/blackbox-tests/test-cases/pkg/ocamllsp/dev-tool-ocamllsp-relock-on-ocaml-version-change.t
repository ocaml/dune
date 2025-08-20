Test that if the version of the "ocaml" package in the project's
lockdir changes then the ocamllsp dev tool is re-locked to be built
with the version of the ocaml compiler now in the project's
lockdir. This is necessary because ocamllsp must be compiled with the
same version of the ocaml compiler as the code that it's analyzing.

  $ . ../helpers.sh
  $ . ./helpers.sh

  $ mkrepo
  $ make_mock_ocamllsp_package
  $ mkpkg ocaml 5.2.0
  $ mkpkg ocaml 5.1.0

  $ setup_ocamllsp_workspace

  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > 
  > (package
  >  (name foo)
  >  (allow_empty))
  > EOF

  $ make_lockdir
  $ make_lockpkg ocaml <<EOF
  > (version 5.2.0)
  > EOF

Initially ocamllsp will be depend on ocaml.5.2.0 to match the project.
  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune tools exec ocamllsp
  Solution for dev-tools.locks/ocaml-lsp-server:
  - ocaml.5.2.0
  - ocaml-lsp-server.0.0.1
       Running 'ocamllsp'
  hello from fake ocamllsp
  $ cat dev-tools.locks/ocaml-lsp-server/ocaml.pkg
  (version 5.2.0)

We can re-run "dune tools exec ocamllsp" without relocking or rebuilding.
  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune tools exec ocamllsp
       Running 'ocamllsp'
  hello from fake ocamllsp

Change the version of ocaml that the project depends on.
  $ make_lockpkg ocaml <<EOF
  > (version 5.1.0)
  > EOF

Running "dune tools exec ocamllsp" causes ocamllsp to be relocked and rebuilt
before running. Ocamllsp now depends on ocaml.5.1.0.
  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune tools exec ocamllsp
  The version of the compiler package ("ocaml") in this project's lockdir has
  changed to 5.1.0 (formerly the compiler version was 5.2.0). The dev-tool
  "ocaml-lsp-server" will be re-locked and rebuilt with this version of the
  compiler.
  Solution for dev-tools.locks/ocaml-lsp-server:
  - ocaml.5.1.0
  - ocaml-lsp-server.0.0.1
       Running 'ocamllsp'
  hello from fake ocamllsp
  $ cat dev-tools.locks/ocaml-lsp-server/ocaml.pkg
  (version 5.1.0)
