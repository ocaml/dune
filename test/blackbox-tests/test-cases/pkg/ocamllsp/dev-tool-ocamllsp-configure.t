Test that adding constraints to ocamllsp via `lock_dir` works.

  $ mkrepo
  $ mk_ocaml 5.2.0
  $ mkpkg ocaml-lsp-server <<EOF
  > depends: ["ocamlbuild"]
  > install: [
  >  [ "sh" "-c" "echo '#!/bin/sh' > %{bin}%/ocamllsp" ]
  >  [ "sh" "-c" "echo 'echo hello from fake ocamllsp' >> %{bin}%/ocamllsp" ]
  >  [ "chmod" "a+x" "%{bin}%/ocamllsp" ]
  > ]
  > EOF

  $ mkpkg ocamlbuild 0.0.1
  $ mkpkg ocamlbuild 0.0.2
  $ mkpkg ocamlbuild 0.0.3

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > 
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends
  >    (ocaml (= 5.2.0))))
  > EOF

Add ocamlbuild 0.0.2 as a constraint to make sure the constraint field makes the solver pick the correct version

  $ cat > dune-workspace << EOF
  > (lang dune 3.20)
  > (pkg enabled)
  > (lock_dir
  >  (path "${dev_tool_lock_dir}")
  >  (constraints
  >   (ocamlbuild (= 0.0.2)))
  >  (repositories mock))
  > (lock_dir
  >  (repositories mock))
  > (repository
  >  (name mock)
  >  (url "file://$(pwd)/mock-opam-repository"))
  > EOF

Installing ocamllsp picks the right version of ocamlbuild

  $ dune tools install ocamllsp
  Solution for _build/.dev-tools.locks/ocaml-lsp-server:
  - ocaml-base-compiler.5.2.0
  - ocaml-compiler.5.2.0
  - ocaml-lsp-server.0.0.1
  - ocamlbuild.0.0.2
