When the version in .ocamlformat changes, automatically relock ocamlformat with
the new version.

  $ . ./helpers.sh
  $ mkrepo

  $ make_fake_ocamlformat "0.26.0"
  $ echo ocamlformat-0.26.0.tar > fake-curls
  $ make_ocamlformat_opam_pkg "0.26.0" 1

  $ make_fake_ocamlformat "0.27.0"
  $ echo ocamlformat-0.27.0.tar >> fake-curls
  $ make_ocamlformat_opam_pkg "0.27.0" 2

Make dune-project that uses the mocked dev-tool opam-reposiotry.
  $ make_project_with_dev_tool_lockdir

Create .ocamlformat file
  $ cat > .ocamlformat <<EOF
  > version = 0.26.0
  > EOF

Install ocamlformat. 0.26.0 should be installed because that's the version in .ocamlformat.
  $ dune tools install ocamlformat
  Solution for _build/.dev-tools.locks/ocamlformat:
  - ocamlformat.0.26.0

Change the version in .ocamlformat.
  $ cat > .ocamlformat <<EOF
  > version = 0.27.0
  > EOF

Install ocamlformat again. Dune should detect that the version has changed and relock:
  $ dune tools install ocamlformat
  The lock directory for the tool "ocamlformat" exists but contains a solution
  for 0.26.0 of the tool, whereas version 0.27.0 now needs to be installed. The
  tool will now be re-locked.
  Solution for _build/.dev-tools.locks/ocamlformat:
  - ocamlformat.0.27.0
