Test that if the version of the "ocaml" package in the project's
lockdir changes then the ocamllsp dev tool is re-locked to be built
with the version of the ocaml compiler now in the project's
lockdir. This is necessary because ocamllsp must be compiled with the
same version of the ocaml compiler as the code that it's analyzing.

  $ mkrepo
  $ make_mock_ocamllsp_package
  $ mk_ocaml 5.2.0
  $ mk_ocaml 5.1.0

  $ setup_ocamllsp_workspace

  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > 
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends
  >    (ocaml (= 5.2.0))))
  > EOF

  $ dune build

Initially ocamllsp will depend on ocaml-base-compiler.5.2.0 to match the project.
  $ dune tools install ocamllsp
  Solution for _build/.dev-tools.locks/ocaml-lsp-server:
  - ocaml-base-compiler.5.2.0
  - ocaml-compiler.5.2.0
  - ocaml-lsp-server.0.0.1

  $ dune tools exec ocamllsp
       Running 'ocamllsp'
  hello from fake ocamllsp
  $ grep "version" "${dev_tool_lock_dir}"/ocaml-base-compiler.pkg
  (version 5.2.0)

We can re-run "dune tools exec ocamllsp" without relocking or rebuilding.
  $ dune tools exec ocamllsp
       Running 'ocamllsp'
  hello from fake ocamllsp

Change the version of ocaml that the project depends on.
  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > 
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends
  >    (ocaml (= 5.1.0))))
  > EOF

  $ dune build


Running "dune tools exec ocamllsp" causes ocamllsp to be relocked and rebuilt
before running. Ocamllsp now depends on ocaml-base-compiler.5.1.0.
  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune tools exec ocamllsp
  The version of the compiler package ("ocaml-base-compiler") in this project's
  lockdir has changed to 5.1.0 (formerly the compiler version was 5.2.0). The
  dev-tool "ocaml-lsp-server" will be re-locked and rebuilt with this version
  of the compiler.
  Solution for _build/.dev-tools.locks/ocaml-lsp-server:
  - ocaml-base-compiler.5.1.0
  - ocaml-compiler.5.1.0
  - ocaml-lsp-server.0.0.1
       Running 'ocamllsp'
  hello from fake ocamllsp
  $ grep "version" "${dev_tool_lock_dir}"/ocaml-base-compiler.pkg
  (version 5.1.0)
