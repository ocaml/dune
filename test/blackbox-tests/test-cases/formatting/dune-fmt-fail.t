`dune fmt` should still fail when the formatter action itself fails. This
must not be classified as a promotion-only failure.

  $ make_dune_project 3.25
  $ cat > dune <<EOF
  > (library
  >  (name foo))
  > EOF
  $ touch .ocamlformat
  $ cat > foo.ml <<EOF
  > let =
  > EOF

Create a formatter that behaves like ocamlformat failing to parse the source.

  $ mkdir bin
  $ cat > bin/ocamlformat <<EOF
  > #!/bin/sh
  > echo "ocamlformat: failed to parse foo.ml" >&2
  > exit 1
  > EOF
  $ chmod +x bin/ocamlformat

  $ PATH="$PWD/bin:$PATH" dune fmt
  File "foo.ml", line 1, characters 0-0:
  ocamlformat: failed to parse foo.ml
  [1]
