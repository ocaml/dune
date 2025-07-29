Test that the ocamllsp dev tool can see the ocamlformat dev tool.

  $ . ../helpers.sh
  $ . ./helpers.sh

  $ mkrepo
  $ mkpkg ocaml 5.2.0

  $ cat > dune-workspace <<EOF
  > (lang dune 3.16)
  > (lock_dir
  >  (path "dev-tools.locks/ocaml-lsp-server")
  >  (repositories mock))
  > (lock_dir
  >  (path "dev-tools.locks/ocamlformat")
  >  (repositories mock))
  > (lock_dir
  >   (repositories mock))
  > (repository
  >  (name mock)
  >  (url "file://$(pwd)/mock-opam-repository"))
  > EOF

Make a fake ocamllsp package that prints out the PATH variable:
  $ mkpkg ocaml-lsp-server <<EOF
  > install: [
  >   [ "sh" "-c" "echo '#!/bin/sh' > %{bin}%/ocamllsp" ]
  >   [ "sh" "-c" "echo 'echo fake ocamllsp will now run fake ocamlformat:' >> %{bin}%/ocamllsp" ]
  >   [ "sh" "-c" "echo 'ocamlformat' >> %{bin}%/ocamllsp" ]
  >   [ "sh" "-c" "chmod a+x %{bin}%/ocamllsp" ]
  > ]
  > EOF

Make a fake ocamlformat
  $ mkpkg ocamlformat <<EOF
  > install: [
  >   [ "sh" "-c" "echo '#!/bin/sh' > %{bin}%/ocamlformat" ]
  >   [ "sh" "-c" "echo 'echo hello from fake ocamlformat' >> %{bin}%/ocamlformat" ]
  >   [ "sh" "-c" "chmod a+x %{bin}%/ocamlformat" ]
  > ]
  > EOF

  $ make_lockdir
  $ cat > dune.lock/ocaml.pkg <<EOF
  > (version 5.2.0)
  > EOF

  $ dune tools install ocamlformat
  Solution for dev-tools.locks/ocamlformat:
  - ocamlformat.0.0.1

  $ dune tools exec ocamllsp
  Solution for dev-tools.locks/ocaml-lsp-server:
  - ocaml.5.2.0
  - ocaml-lsp-server.0.0.1
       Running 'ocamllsp'
  fake ocamllsp will now run fake ocamlformat:
  hello from fake ocamlformat
