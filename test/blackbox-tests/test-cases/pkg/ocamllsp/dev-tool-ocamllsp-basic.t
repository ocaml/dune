Test that the "dune tools exec ocamllsp" command runs ocamllsp after it
has been installed with "dune tools install ocamllsp".

  $ mkrepo
  $ make_mock_ocamllsp_package
  $ mk_ocaml 5.2.0
  $ setup_ocamllsp_workspace

  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > 
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends
  >   (ocaml (= 5.2.0))))
  > EOF

  $ dune build

First install the tool:

  $ dune tools install ocamllsp
  Solution for _build/.dev-tools.locks/ocaml-lsp-server:
  - ocaml-base-compiler.5.2.0
  - ocaml-compiler.5.2.0
  - ocaml-lsp-server.0.0.1

Then exec runs it:

  $ dune tools exec ocamllsp
       Running 'ocamllsp'
  hello from fake ocamllsp

Make sure that after evaling the output of 'dune tools env', the first ocamllsp
executable in PATH is the one installed by dune as a dev tool.
  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled eval $(dune tools env)
  $ which ocamllsp
  $TESTCASE_ROOT/_build/_private/default/.dev-tool/ocaml-lsp-server/target/bin/ocamllsp
