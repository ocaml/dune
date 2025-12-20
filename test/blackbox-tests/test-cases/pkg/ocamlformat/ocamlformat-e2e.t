Exercises end to end, locking and building ocamlformat dev tool.

  $ mkrepo

  $ make_fake_ocamlformat "0.26.2"
  $ make_fake_ocamlformat "0.26.3"

Add the tar file for the fake curl to copy it:
  $ echo ocamlformat-0.26.2.tar > fake-curls
  $ PORT=1

  $ make_ocamlformat_opam_pkg "0.26.2" $PORT

Add the tar file for the fake curl to copy it:
  $ echo ocamlformat-0.26.3.tar >> fake-curls
  $ PORT=2

We consider this version of OCamlFormat as the latest version:
  $ make_ocamlformat_opam_pkg "0.26.3" $PORT

Make dune-project that uses the mocked dev-tool opam-reposiotry.
  $ make_project_with_dev_tool_lockdir

Without a ".ocamlformat" file, "dune fmt" takes the latest version of
OCamlFormat.
  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune fmt --preview
  Solution for _build/.dev-tools.locks/ocamlformat:
  - ocamlformat.0.26.3
  File "foo.ml", line 1, characters 0-0:
  Error: Files _build/default/foo.ml and _build/default/.formatted/foo.ml
  differ.
  [1]
  $ cat _build/default/.formatted/foo.ml
  formatted with version 0.26.3

Create .ocamlformat file
  $ cat > .ocamlformat <<EOF
  > version = 0.26.2
  > EOF

An important cleaning here, "dune fmt" will relock and build the new version(0.26.2) of OCamlFormat.
  $ rm -r "${dev_tool_lock_dir}"
  $ dune clean

With a ".ocamlformat" file, "dune fmt" takes the version mentioned inside ".ocamlformat"
file.
  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune fmt --preview
  Solution for _build/.dev-tools.locks/ocamlformat:
  - ocamlformat.0.26.2
  File "foo.ml", line 1, characters 0-0:
  Error: Files _build/default/foo.ml and _build/default/.formatted/foo.ml
  differ.
  [1]
  $ cat _build/default/.formatted/foo.ml
  formatted with version 0.26.2

Formating a second time would not trigger the lock/solve.
  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune fmt --preview
  File "foo.ml", line 1, characters 0-0:
  Error: Files _build/default/foo.ml and _build/default/.formatted/foo.ml
  differ.
  [1]
  $ cat _build/default/.formatted/foo.ml
  formatted with version 0.26.2

When the lock dir is removed, the solving/lock is renewed:

  $ rm -r "${dev_tool_lock_dir}"
  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune fmt --preview
  Solution for _build/.dev-tools.locks/ocamlformat:
  - ocamlformat.0.26.2
  File "foo.ml", line 1, characters 0-0:
  Error: Files _build/default/foo.ml and _build/default/.formatted/foo.ml
  differ.
  [1]
