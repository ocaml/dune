Test that the ocamllsp dev tool executes in an environment where other dev
tools are in PATH.

  $ mkrepo
  $ mk_ocaml 5.2.0
  $ setup_ocamllsp_workspace

Make a fake ocamllsp package that prints out the PATH variable:
  $ mkpkg ocaml-lsp-server <<EOF
  > install: [
  >   [ "sh" "-c" "echo '#!/bin/sh' > %{bin}%/ocamllsp" ]
  >   [ "sh" "-c" "echo 'echo \$PATH' >> %{bin}%/ocamllsp" ]
  >   [ "sh" "-c" "chmod a+x %{bin}%/ocamllsp" ]
  > ]
  > EOF

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

First install the tool:
  $ dune tools install ocamllsp
  Solution for _build/.dev-tools.locks/ocaml-lsp-server:
  - ocaml-base-compiler.5.2.0
  - ocaml-compiler.5.2.0
  - ocaml-lsp-server.0.0.1

Confirm that each dev tool's bin directory is now in PATH:
  $ dune tools exec ocamllsp | tr : '\n' | grep '_build/_private/default/.dev-tool'
       Running 'ocamllsp'
  $TESTCASE_ROOT/_build/_private/default/.dev-tool/merlin/target/bin
  $TESTCASE_ROOT/_build/_private/default/.dev-tool/ocaml-index/target/bin
  $TESTCASE_ROOT/_build/_private/default/.dev-tool/dune-release/target/bin
  $TESTCASE_ROOT/_build/_private/default/.dev-tool/opam-publish/target/bin
  $TESTCASE_ROOT/_build/_private/default/.dev-tool/odig/target/bin
  $TESTCASE_ROOT/_build/_private/default/.dev-tool/earlybird/target/bin
  $TESTCASE_ROOT/_build/_private/default/.dev-tool/utop/target/bin
  $TESTCASE_ROOT/_build/_private/default/.dev-tool/ocaml-lsp-server/target/bin
  $TESTCASE_ROOT/_build/_private/default/.dev-tool/odoc/target/bin
  $TESTCASE_ROOT/_build/_private/default/.dev-tool/ocamlformat/target/bin
