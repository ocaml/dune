The corrections field is rejected before Dune 3.23

  $ make_dune_project 3.22
  $ cat > dune <<'EOF'
  > (rule
  >  (alias runtest)
  >  (corrections produce)
  >  (action (system "echo corrected > a.corrected")))
  > EOF
  $ dune build @runtest
  File "dune", line 3, characters 1-22:
  3 |  (corrections produce)
       ^^^^^^^^^^^^^^^^^^^^^
  Error: 'corrections' is only available since version 3.23 of the dune
  language. Please update your dune-project file to have (lang dune 3.23).
  [1]
