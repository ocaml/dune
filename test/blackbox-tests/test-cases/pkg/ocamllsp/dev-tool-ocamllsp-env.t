Check that `dune tools exec ocamllsp` can call the dune tools version of ocamlformat:

  $ . ../helpers.sh
  $ . ./helpers.sh

  $ mkrepo
  $ mkpkg ocaml 5.2.0
  $ mkpkg ocaml-lsp-server <<EOF
  > install: [
  >   [ "sh" "-c" "echo '#!/bin/sh' > %{bin}%/ocamllsp" ]
  >   [ "sh" "-c" "echo 'echo hello from fake ocamllsp' >> %{bin}%/ocamllsp" ]
  >   [ "sh" "-c" "echo ocamlformat >> %{bin}%/ocamllsp" ]
  >   [ "sh" "-c" "chmod a+x %{bin}%/ocamllsp" ]
  > ]
  > EOF
  $ mkpkg ocamlformat <<EOF
  > install: [
  >   [ "sh" "-c" "echo '#!/bin/sh' > %{bin}%/ocamlformat" ]
  >   [ "sh" "-c" "echo 'echo hello from fake ocamlformat' >> %{bin}%/ocamlformat" ]
  >   [ "sh" "-c" "chmod a+x %{bin}%/ocamlformat" ]
  > ]
  > EOF

  $ setup_ocamllsp_workspace

  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > EOF

  $ make_lockdir
  $ cat > dune.lock/ocaml.pkg <<EOF
  > (version 5.2.0)
  > EOF

  $ dune tools exec ocamllsp
  Solution for dev-tools.locks/ocaml-lsp-server:
  - ocaml.5.2.0
  - ocaml-lsp-server.0.0.1
       Running 'ocamllsp'
  hello from fake ocamllsp
  Solution for dev-tools.locks/ocamlformat:
  - ocamlformat.0.0.1
       Running 'ocamlformat'
  hello from fake ocamlformat

Users can also configure their PATH variable environment:

  $ eval $(dune tools env)
  $ echo $PATH | sed 's/:.*//'
  $TESTCASE_ROOT/_build/_private/default/.dev-tool/bin
  $ which ocamllsp
  $TESTCASE_ROOT/_build/_private/default/.dev-tool/bin/ocamllsp
  $ which ocamlformat
  $TESTCASE_ROOT/_build/_private/default/.dev-tool/bin/ocamlformat
  $ ocamllsp
       Running 'ocamllsp'
  hello from fake ocamllsp
       Running 'ocamlformat'
  hello from fake ocamlformat
