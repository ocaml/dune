Test that the ocamllsp dev tool can see the ocamlformat dev tool.

  $ mkrepo
  $ mk_ocaml 5.2.0
  $ setup_ocamllsp_workspace
  $ cat >> dune-workspace <<EOF
  > (lock_dir
  >  (path "_build/.dev-tools.locks/ocamlformat")
  >  (repositories mock))
  > EOF

Make a fake ocamllsp package that invokes ocamlformat:
  $ mkpkg ocaml-lsp-server <<EOF
  > depends: [ "ocaml" ]
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

  $ make_named_package_project foo 3.20 "(ocaml (= 5.2.0))"
  $ dune build

  $ dune tools install ocamlformat
  Solution for _build/.dev-tools.locks/ocamlformat:
  - ocamlformat.0.0.1

  $ dune tools exec ocamllsp
  Solution for _build/.dev-tools.locks/ocaml-lsp-server:
  - ocaml.5.2.0
  - ocaml-base-compiler.5.2.0
  - ocaml-compiler.5.2.0
  - ocaml-lsp-server.0.0.1
       Running 'ocamllsp'
  fake ocamllsp will now run fake ocamlformat:
  hello from fake ocamlformat
