Dune should fail to parse synopsis for versions below 3.19

  $ cat > dune-project << EOF
  > (lang dune 3.18)
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
  File "dune", line 4, characters 1-61:
  4 |  (synopsis "Synopsis for rule with alias rule-with-synopsis")
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: 'synopsis' is only available since version 3.19 of the dune language.
  Please update your dune-project file to have (lang dune 3.19).
