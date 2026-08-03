Custom alias for the cinaps

  $ make_cinaps_project 3.7 1.2

  $ cat > dune <<EOF
  > (cinaps
  >  (files foo.ml)
  >  (alias foo))
  > EOF

  $ touch foo.ml

  $ dune build @foo
  $ dune trace cat | jq_dune -c 'processesBrief | select(.prog == "cinaps")' | censor
  {"prog":"cinaps","args":["-staged",".cinaps.$CINAPS/cinaps.ml-gen","foo.ml"],"exit":0}
