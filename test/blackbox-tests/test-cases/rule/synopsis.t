Dune should parse rule with alias with synopsis. TODO: version 3.19?

  $ cat > dune-project << EOF
  > (lang dune 3.18)
  > EOF
  $ cat > dune << EOF
  > (rule
  >  (targets touched-file.ml)
  >  (alias rule-with-synopsis)
  >  (synopsis "Synopsis for rule with alias rule-with-synopsis")
  >  (action
  >   (write-file touched-file.ml "Echo from rule with alias rule-with-synopsis")))
  > EOF

  $ dune build @rule-with-synopsis
  $ cat _build/default/touched-file.ml
  Echo from rule with alias rule-with-synopsis
  $ dune show targets
