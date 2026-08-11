When an OCamlFormat version does not exist, "dune fmt" would fail with a
solving error.

  $ mkrepo

Make a project with no dependency on OCamlFormat.
  $ make_project_with_dev_tool_lockdir

Update ".ocamlformat" file with unknown version of OCamlFormat.
  $ cat > .ocamlformat <<EOF
  > version = 0.26.9
  > EOF

Format, it shows the solving error.
  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune fmt
  Error:
  Unable to solve dependencies while generating lock directory:
  $TESTCASE_ROOT/_build/.dev-tools.locks/ocamlformat
  
  The dependency solver failed to find a solution for the requested platforms:
  - arch = x86_64; os = linux
  - arch = arm64; os = linux
  - arch = x86_64; os = macos
  - arch = arm64; os = macos
  ...with this error:
  Couldn't solve the package dependency formula.
  The following packages couldn't be found: ocamlformat
  [1]
