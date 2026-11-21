Test that ocamllsp uses a pinned compiler branch instead of fetching from
opam-repository.

  $ mkrepo
  $ make_mock_ocamllsp_package
  $ mk_ocaml 5.2.0

Create a fake pinned compiler source. This simulates a scenario where the user
has pinned a custom compiler branch (e.g., OxCaml).

  $ mkdir -p pinned-compiler
  $ cat > pinned-compiler/ocaml-base-compiler.opam <<EOF
  > opam-version: "2.0"
  > version: "5.2.0+custom"
  > flags: compiler
  > conflict-class: "ocaml-core-compiler"
  > EOF

Set up the dune-project with a pin for the ocaml-base-compiler pointing to our custom source.

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > (pin
  >  (url "file://$PWD/pinned-compiler")
  >  (package (name ocaml-base-compiler)))
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends ocaml-base-compiler))
  > EOF

  $ setup_ocamllsp_workspace

Lock the project first to establish the pinned compiler in the lockfile.

  $ dune pkg lock 2>&1 | head -20
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  - ocaml-base-compiler.dev



Check the lockfile to verify the compiler is set.

  $ cat dune.lock/lock.dune | grep ocaml
  (ocaml ocaml-base-compiler)

Now install ocamllsp. It should use the pinned compiler, not fetch from opam-repository.
The key indicator is that we see "ocaml-base-compiler.dev" in the solution (from our pin),
rather than "ocaml-base-compiler.5.2.0" from opam-repository.

  $ dune tools install ocamllsp 2>&1 | head -10
  Solution for _build/.dev-tools.locks/ocaml-lsp-server:
  - ocaml-base-compiler.dev
  - ocaml-lsp-server.0.0.1

  $ dune tools exec ocamllsp
  The version of the compiler package ("ocaml-base-compiler") in this project's
  lockdir has changed to dev (formerly the compiler version was dev). The
  dev-tool "ocaml-lsp-server" will be re-locked and rebuilt with this version
  of the compiler.
  Solution for _build/.dev-tools.locks/ocaml-lsp-server:
  - ocaml-base-compiler.dev
  - ocaml-lsp-server.0.0.1
       Running 'ocamllsp'
  hello from fake ocamllsp

A pinned compiler may itself depend on a package that exists only as a project
pin. The tool solve needs that compiler pin, but not unrelated project pins.

  $ mkdir compiler-support
  $ cat >compiler-support/compiler-support.opam <<'EOF'
  > opam-version: "2.0"
  > EOF
  $ cat >>dune-project <<EOF
  > (pin
  >  (url "file://$PWD/compiler-support")
  >  (package (name compiler-support)))
  > EOF
  $ echo 'depends: [ "compiler-support" ]' >>pinned-compiler/ocaml-base-compiler.opam
  $ dune pkg lock >project-solve 2>&1
  $ grep -q compiler-support project-solve
  $ rm -rf "$dev_tool_lock_dir"
  $ if dune tools exec ocamllsp >tool-solve 2>&1; then
  >   tail -n 1 tool-solve
  > elif grep -q compiler-support tool-solve; then
  >   echo 'compiler dependency pin missing'
  > else
  >   cat tool-solve
  > fi
  compiler dependency pin missing
