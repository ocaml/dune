Sandboxed corrected files fail and register promotions from Dune 3.23

  $ make_dune_project 3.23
  $ echo original > a
  $ cat > dune <<'EOF'
  > (rule
  >  (alias runtest)
  >  (corrections produce)
  >  (deps a)
  >  (action (system "echo corrected > a.corrected")))
  > EOF
  $ dune build @runtest
  File "a", line 1, characters 0-0:
  --- a
  +++ _build/default/a.corrected
  @@ -1 +1 @@
  -original
  +corrected
  [1]
  $ dune promotion list
  a
  $ dune promote
  Promoting _build/default/a.corrected to a.
  $ cat a
  corrected
