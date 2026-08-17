Generated META values currently leave backslashes unescaped. Snapshot both the
output and its failure to round-trip through findlib.

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
  description = "ends in \"

  $ export OCAMLPATH="$PWD/_build/install/default/lib"
  $ export OCAMLFIND_LDCONF=ignore
  $ ocamlfind query -format '%D' slashlib
  ocamlfind: While parsing '$TESTCASE_ROOT/_build/install/default/lib/slashlib/META': Expected 'name = value' clause at line 1 position 38
  [2]
