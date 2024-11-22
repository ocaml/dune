Exercise the `dune tools install ocamlformat` command.

  $ . ../helpers.sh
  $ . ./helpers.sh


  $ make_fake_ocamlformat "0.26.2"
  $ make_ocamlformat_opam_pkg "0.26.2"
  $ make_project_with_dev_tool_lockdir

  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > 
  > (package
  >  (name foo)
  >  (allow_empty))
  > EOF

Install ocamlformat but do not run it.
  $ dune tools install ocamlformat
  Solution for dev-tools.locks/ocamlformat:
  - ocamlformat.0.26.2

Run ocamlformat (no solving or building necessary).
  $ dune tools exec ocamlformat
       Running 'ocamlformat'
  formatted with version 0.26.2
