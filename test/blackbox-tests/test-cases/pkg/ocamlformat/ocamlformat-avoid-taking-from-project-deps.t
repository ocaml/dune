If the dev-tool feature is enabled then "dune fmt" should invoke the "ocamlformat"
executable from the dev-tool and not the one from the project's regular package
dependencies.

If the dev-tool feature is not enabled then "dune fmt" should invoke the
"ocamlformat" executable from the project's regular package dependencies.

  $ mkrepo

  $ make_fake_ocamlformat "0.26.2"
  $ make_fake_ocamlformat "0.26.3"

  $ make_ocamlformat_opam_pkg "0.26.2"
  $ make_ocamlformat_opam_pkg "0.26.3"

Make a project that depends on the fake ocamlformat.0.26.2:
  $ make_project_with_dev_tool_lockdir

Update dune-project to add the dependency on OCamlFormat.
  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (package
  >  (name foo)
  >  (depends (ocamlformat (= 0.26.2))))
  > EOF

Lock and build the project to make OCamlFormat from the project dependencies available.
  $ dune_pkg_lock_normalized
  Solution for dune.lock:
  - ocamlformat.0.26.2

Run "dune fmt" without the dev-tools feature enabled. This should invoke the ocamlformat
executable from the package dependencies (ie., 'ocamlformat.0.26.2').
  $ dune fmt --preview
  File "foo.ml", line 1, characters 0-0:
  Error: Files _build/default/foo.ml and _build/default/.formatted/foo.ml
  differ.
  [1]
  $ cat _build/default/.formatted/foo.ml
  formatted with version 0.26.2

Format using the dev-tools feature, it does not invoke the OCamlFormat binary from
the project dependencies (0.26.2) but instead builds and runs the OCamlFormat binary as a
dev-tool (0.26.3).
  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune fmt
  Solution for _build/.dev-tools.locks/ocamlformat:
  - ocamlformat.0.26.3
  File "foo.ml", line 1, characters 0-0:
  Error: Files _build/default/foo.ml and _build/default/.formatted/foo.ml
  differ.
  Promoting _build/default/.formatted/foo.ml to foo.ml.
  [1]
  $ cat foo.ml
  formatted with version 0.26.3

Retry, without dev-tools feature and without cleaning. This time it uses the OCamlFormat
binary from the project dependencies rather than the dev-tool. This exercises the
behavior when OCamlFormat is installed simultaneously as both a dev-tool and as a
regular package dependency.
  $ rm -r "${dev_tool_lock_dir}"
  $ dune fmt --preview
  File "foo.ml", line 1, characters 0-0:
  Error: Files _build/default/foo.ml and _build/default/.formatted/foo.ml
  differ.
  [1]
  $ cat _build/default/.formatted/foo.ml
  formatted with version 0.26.2
