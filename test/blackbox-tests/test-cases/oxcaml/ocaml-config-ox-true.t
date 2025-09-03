Check that %{ocaml-config:ox} works and returns true

  $ cat > "dune-project" <<EOF
  > (lang dune 1.0)
  > EOF

  $ cat > "dune" <<EOF
  > (rule
  >   (targets x)
  >     (action (with-stdout-to %{targets} (echo %{ocaml-config:ox}))))

  $ dune build x
  $ cat _build/default/x
  true
