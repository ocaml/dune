Showcases issues with ocamldep when using melange ppx

The code in main contains melange-only attributes, resolved by its ppx

  $ cat > a.ml <<EOF
  > let t = "foo"
  > EOF

  $ cat > main.ml <<EOF
  > let t = [%bs.obj { foo = A.t }]
  > EOF

Ocamldep does not show dependencies on A

  $ ocamldep -I . main.ml
  main.cmo :
  main.cmx :

Change main to remove melange-specific ppx:

  $ cat > main.ml <<EOF
  > let t = A.t
  > EOF

Now ocamldep does show dependency on A

  $ ocamldep -I . main.ml
  main.cmo : \
      a.cmo
  main.cmx : \
      a.cmx
