Test that ocamllsp uses a pinned compiler branch instead of fetching from
opam-repository.

  $ mkrepo
  $ make_mock_ocamllsp_package
  $ mk_ocaml 5.2.0

Create a fake pinned compiler source. This simulates a scenario where the user
has pinned a custom compiler branch (e.g., OxCaml).

  $ mkdir -p pinned-compiler
  $ cat > pinned-compiler/ocaml.opam <<EOF
  > opam-version: "2.0"
  > version: "5.2.0+custom"
  > flags: compiler
  > conflict-class: "ocaml-core-compiler"
  > EOF

Set up the dune-project with a pin for the ocaml compiler pointing to our custom source.

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > (pin
  >  (url "file://$PWD/pinned-compiler")
  >  (package (name ocaml)))
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends ocaml))
  > EOF

  $ setup_ocamllsp_workspace

Lock the project first to establish the pinned compiler in the lockfile.

  $ dune pkg lock 2>&1 | head -20
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  - ocaml.dev



Check the lockfile to verify the compiler is set.

  $ cat dune.lock/lock.dune | grep ocaml
  (ocaml ocaml)

Now install ocamllsp. It should use the pinned compiler, not fetch from opam-repository.
The key indicator is that we see "ocaml.dev" in the solution (from our pin),
rather than "ocaml-base-compiler.5.2.0" from opam-repository.

  $ dune tools install ocamllsp 2>&1 | head -10
  Solution for _build/.dev-tools.locks/ocaml-lsp-server:
  - ocaml.dev
  - ocaml-lsp-server.0.0.1

  $ dune tools exec ocamllsp
  The version of the compiler package ("ocaml") in this project's lockdir has
  changed to dev (formerly the compiler version was dev). The dev-tool
  "ocaml-lsp-server" will be re-locked and rebuilt with this version of the
  compiler.
  Solution for _build/.dev-tools.locks/ocaml-lsp-server:
  - ocaml.dev
  - ocaml-lsp-server.0.0.1
       Running 'ocamllsp'
  hello from fake ocamllsp
