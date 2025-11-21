Test that the "dune tools exec ocamllsp" command causes ocamllsp to be
locked, built and run when the command is run from a dune project with
a lockdir containing an "ocaml" lockfile.

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
  $ make_lockpkg ocaml <<EOF
  > (version 5.2.0)
  > EOF

  $ dune tools exec ocamllsp
  Solution for _build/.dev-tools.locks/ocaml-lsp-server:
  - ocaml.5.2.0
  - ocaml-lsp-server.0.0.1
       Running 'ocamllsp'
  hello from fake ocamllsp

Make sure that after evaling the output of 'dune tools env', the first ocamllsp
executable in PATH is the one installed by dune as a dev tool.
  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled eval $(dune tools env)
  $ which ocamllsp
  $TESTCASE_ROOT/_build/_private/default/.dev-tool/ocaml-lsp-server/target/bin/ocamllsp
