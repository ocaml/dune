Dune should parse rule with alias with synopsis.
Dune should show synopsis for targets.
Dune should show synopsis for alias.

  $ cat > dune-project << EOF
  > (lang dune 3.19)
  > EOF
  $ cat > dune << EOF
  > (rule
  >  (target touched-file.ml)
  >  (alias rule-with-synopsis)
  >  (synopsis "Synopsis for rule with alias rule-with-synopsis")
  >  (action
  >   (write-file %{target} "")))
  > EOF

  $ dune show targets
  dune
  dune-project
  touched-file.ml
    - dune:1 Synopsis for rule with alias rule-with-synopsis
  $ dune show aliases
  all
  default
  fmt
  ocaml-index
  pkg-install
  rule-with-synopsis
    - dune:1 Synopsis for rule with alias rule-with-synopsis
