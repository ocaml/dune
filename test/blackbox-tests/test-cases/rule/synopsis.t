Dune should parse rule with alias with synopsis. TODO: version 3.19?

  $ cat > dune-project << EOF
  > (lang dune 3.18)
  > EOF
  $ cat > dune << EOF
  > (rule
  >  (alias rule-with-synopsis)
  >  (synopsis "Synopsis for rule with alias rule-with-synopsis")
  >  (action
  >   (echo "Echo from rule with alias rule-with-synopsis")))
  > EOF

  $ dune build @rule-with-synopsis
  Echo from rule with alias rule-with-synopsis
