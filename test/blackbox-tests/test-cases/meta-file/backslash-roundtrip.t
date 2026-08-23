Generated META values escape backslashes so the output round-trips through
findlib without changing the decoded value.

  $ make_dune_project_with_package 2.7 slashlib

  $ cat >slashlib.ml <<EOF
  > let foo () = ()
  > EOF

  $ cat >dune <<'EOF'
  > (library
  >  (public_name slashlib)
  >  (synopsis "ends in \\"))
  > EOF

  $ dune build @install
  $ grep '^description' _build/default/META.slashlib
  description = "ends in \\"

  $ export OCAMLPATH="$PWD/_build/install/default/lib"
  $ export OCAMLFIND_LDCONF=ignore
  $ ocamlfind query -format '%D' slashlib
  ends in \
