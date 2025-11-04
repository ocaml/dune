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

Format, it shows the solving error.
  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune fmt
  Error: Unable to solve dependencies for the following lock directories:
  Lock directory _build/.dev-tools.locks/ocamlformat:
  Couldn't solve the package dependency formula.
  The following packages couldn't be found: ocamlformat
  [1]
