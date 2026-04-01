Explicit corrections ignore leaves corrected files alone

  $ make_dune_project 3.23
  $ echo original > a
  $ cat > dune <<'EOF'
  > (rule
  >  (alias runtest)
  >  (corrections ignore)
  >  (deps a)
  >  (action (system "echo corrected > a.corrected")))
  > EOF
  $ dune build @runtest && echo passed
  passed
  $ dune promotion list
  $ cat a
  original
