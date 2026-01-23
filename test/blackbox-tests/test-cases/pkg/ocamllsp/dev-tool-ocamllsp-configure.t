Test that adding constraints to ocamllsp via `lock_dir` works.

  $ mkrepo
  $ mk_ocaml 5.2.0
  $ mkpkg ocaml-lsp-server <<EOF
  > depends: ["ocamlbuild"]
  > build: [
  >  ["true"]
  > ]
  > install: [
  >  [ "sh" "-c" "echo '#!/bin/sh' > %{bin}%/ocamllsp" ]
  >  [ "sh" "-c" "echo 'echo hello from fake ocamllsp' >> %{bin}%/ocamllsp" ]
  >  [ "chmod a+x %{bin}%/ocamllsp" ]
  > ]
  > EOF

  $ mkpkg ocamlbuild 0.15.0+ox <<EOF
  > build: [
  >  ["true"]
  > ]
  > patches: ["fix.patch"]
  > extra-files: [
  >  "fix.patch"
  > ]
  > EOF

Add patch files for ocamlbuild

  $ mkdir $mock_packages/ocamlbuild/ocamlbuild.0.15.0+ox/files
  $ cat > $mock_packages/ocamlbuild/ocamlbuild.0.15.0+ox/files/fix.patch <<EOF
  > diff --git a/foo b/foo
  > new file mode 100644
  > index 0000000..1111111
  > --- /dev/null
  > +++ b/foo
  > @@ -0,0 +1 @@
  > +patched
  > EOF

Make a mock package to add as pin dependency

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > 
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends
  >    (ocaml (= 5.2.0))))
  > EOF

  $ cat > dune-workspace << EOF
  > (lang dune 3.20)
  > (pkg enabled)
  > (lock_dir
  >  (path "${dev_tool_lock_dir}")
  >  (constraints
  >   (ocamlbuild (= 0.15.0+ox)))
  >  (repositories mock))
  > (lock_dir
  >  (repositories mock))
  > (repository
  >  (name mock)
  >  (url "file://$(pwd)/mock-opam-repository"))
  > EOF

  $ dune build

  $ dune tools install ocamllsp 2>&1 \
  > | dune_cmd subst '_build/\.sandbox/[0-9a-f]+' '_build/.sandbox/HASH' \
  > | dune_cmd subst '(ocamlbuild\.0\.15\.0\+ox)-[0-9a-f]+' 'ocamlbuild.0.15.0+ox-HASH1'
  Solution for _build/.dev-tools.locks/ocaml-lsp-server:
  - ocaml-base-compiler.5.2.0
  - ocaml-compiler.5.2.0
  - ocaml-lsp-server.0.0.1
  - ocamlbuild.0.15.0+ox
  Error:
  open(_build/.sandbox/HASH/_private/default/.pkg/ocamlbuild.0.15.0+ox-HASH1/source/fix.patch): No such file or directory
  -> required by
     _build/_private/default/.pkg/ocamlbuild.0.15.0+ox-HASH1/target/cookie
  -> required by Computing closure for package "ocaml-lsp-server"
  [1]
