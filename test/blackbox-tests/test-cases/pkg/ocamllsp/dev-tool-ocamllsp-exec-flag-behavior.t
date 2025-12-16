Test the behavior of "dune tools exec" with and without the
DUNE_CONFIG__LOCK_DEV_TOOL flag.

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

Without the flag (default behavior), exec fails if tool is not installed:
  $ dune tools exec ocamllsp
  Error: The tool ocamllsp is not installed.
  Hint: Try running 'dune tools install ocamllsp'
  [1]

With DUNE_CONFIG__LOCK_DEV_TOOL=enabled, exec auto-installs and runs:
  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune tools exec ocamllsp
  Solution for _build/.dev-tools.locks/ocaml-lsp-server:
  - ocaml.5.2.0
  - ocaml-lsp-server.0.0.1
       Running 'ocamllsp'
  hello from fake ocamllsp

Now that the tool is installed, exec works without the flag:
  $ dune tools exec ocamllsp
       Running 'ocamllsp'
  hello from fake ocamllsp

With DUNE_CONFIG__LOCK_DEV_TOOL=disabled, exec also fails if tool were not installed
(but since it's already installed, it runs):
  $ DUNE_CONFIG__LOCK_DEV_TOOL=disabled dune tools exec ocamllsp
       Running 'ocamllsp'
  hello from fake ocamllsp
