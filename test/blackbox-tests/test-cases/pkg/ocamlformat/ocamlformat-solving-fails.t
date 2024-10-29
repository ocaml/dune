When an OCamlFormat version does not exist, "dune fmt" would fail with a
solving error.

  $ . ./helpers.sh
  $ mkrepo

Make a project with no dependency on OCamlFormat.
  $ make_project_with_dev_tool_lockdir

Update ".ocamlformat" file with unknown version of OCamlFormat.
  $ cat > .ocamlformat <<EOF
  > version = 0.26.9
  > EOF

The project depends on ocaml, so provide a fake ocaml package:
  $ make_ocaml_opam_pkg

The ocamlformat dev tool requires the project to be locked:
  $ dune pkg lock
  Solution for dune.lock:
  - ocaml.0.0.1

Format, it shows the solving error.
  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune fmt
  Error: Unable to solve dependencies for the following lock directories:
  Lock directory dev-tools.locks/ocamlformat:
  Couldn't solve the package dependency formula.
  The following packages couldn't be found: ocamlformat
  [1]
