Check that %{ocaml-config:ox} doesn't fail. This test does not explicitly
check the value of the parameter.

  $ cat > "dune-project" <<EOF
  > (lang dune 1.0)
  > EOF

  $ cat > "dune" <<EOF
  > (rule
  >   (targets x)
  >     (action (with-stdout-to %{targets} (echo %{ocaml-config:ox}))))

  $ dune build x
