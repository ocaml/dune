Identical sandboxed corrections pass without creating promotions

  $ make_dune_project 3.23
  $ echo same > a
  $ cat > dune <<'EOF'
  > (rule
  >  (alias runtest)
  >  (corrections produce)
  >  (deps a)
  >  (action (system "cp a a.corrected")))
  > EOF
  $ dune build @runtest && echo passed
  passed
  $ dune promotion list
  $ cat a
  same
