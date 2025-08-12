Test that the ocamllsp dev tool executes in an environment where other dev
tools are in PATH.

  $ . ../helpers.sh
  $ . ./helpers.sh

  $ mkrepo
  $ mkpkg ocaml 5.2.0
  $ setup_ocamllsp_workspace

Make a fake ocamllsp package that prints out the PATH variable:
  $ mkpkg ocaml-lsp-server <<EOF
  > install: [
  >   [ "sh" "-c" "echo '#!/bin/sh' > %{bin}%/ocamllsp" ]
  >   [ "sh" "-c" "echo 'echo \$PATH' >> %{bin}%/ocamllsp" ]
  >   [ "sh" "-c" "chmod a+x %{bin}%/ocamllsp" ]
  > ]
  > EOF

  $ make_lockdir
  $ make_lockpkg ocaml <<EOF
  > (version 5.2.0)
  > EOF

Confirm that each dev tool's bin directory is now in PATH:
  $ dune tools exec ocamllsp | tr : '\n' | grep '_build/_private/default/.dev-tool'
  Solution for dev-tools.locks/ocaml-lsp-server:
  - ocaml.5.2.0
  - ocaml-lsp-server.0.0.1
       Running 'ocamllsp'
  $TESTCASE_ROOT/_build/_private/default/.dev-tool/odig/odig/target/bin
  $TESTCASE_ROOT/_build/_private/default/.dev-tool/earlybird/earlybird/target/bin
  $TESTCASE_ROOT/_build/_private/default/.dev-tool/utop/utop/target/bin
  $TESTCASE_ROOT/_build/_private/default/.dev-tool/ocaml-lsp-server/ocaml-lsp-server/target/bin
  $TESTCASE_ROOT/_build/_private/default/.dev-tool/odoc/odoc/target/bin
  $TESTCASE_ROOT/_build/_private/default/.dev-tool/ocamlformat/ocamlformat/target/bin
