Make sure the format rules depends on ".ocamlformat-ignore" file when it exists.

  $ . ./helpers.sh
  $ mkrepo

  $ make_fake_ocamlformat "0.26.2"
  $ make_ocamlformat_opam_pkg "0.26.2"

Make a project that uses the fake ocamlformat:
  $ make_project_with_dev_tool_lockdir

Add a fake binary in the PATH
  $ make_fake_ocamlformat_from_path
  $ which ocamlformat
  $TESTCASE_ROOT/.bin/ocamlformat

Check without ".ocamlformat-ignore" file and the feature.
  $ dune fmt --preview
  File "foo.ml", line 1, characters 0-0:
  Error: Files _build/default/foo.ml and _build/default/.formatted/foo.ml
  differ.
  [1]
  $ cat _build/default/.formatted/foo.ml
  fake ocamlformat from PATH

Create ".ocamlformat-ignore"
  $ touch .ocamlformat-ignore

Check with the feature when ".ocamlformat-ignore" file exists.
  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune fmt --preview
  Solution for _build/.dev-tools.locks/ocamlformat:
  - ocamlformat.0.26.2
  File "foo.ml", line 1, characters 0-0:
  Error: Files _build/default/foo.ml and _build/default/.formatted/foo.ml
  differ.
  [1]
  $ ls _build/default/.ocamlformat-ignore
  _build/default/.ocamlformat-ignore
  $ cat _build/default/.formatted/foo.ml
  ignoring some files
  formatted with version 0.26.2

An important cleaning here, "dune fmt" takes the dev-tool when the lock directory
exists even if the dev-tool feature is disabled.
  $ rm -r "${dev_tool_lock_dir}"

Check without the feature when ".ocamlformat-ignore" file exists.
  $ DUNE_CONFIG__LOCK_DEV_TOOL=disabled dune fmt
  File "foo.ml", line 1, characters 0-0:
  Error: Files _build/default/foo.ml and _build/default/.formatted/foo.ml
  differ.
  Promoting _build/default/.formatted/foo.ml to foo.ml.
  [1]
  $ ls _build/default/.ocamlformat-ignore
  _build/default/.ocamlformat-ignore
  $ cat foo.ml
  ignoring some files
  fake ocamlformat from PATH
