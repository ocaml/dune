All sandbox-produced corrected files are registered

  $ make_dune_project 3.23
  $ mkdir -p sub
  $ echo alpha > a
  $ echo beta > sub/b
  $ cat > dune <<'EOF'
  > (rule
  >  (alias runtest)
  >  (corrections produce)
  >  (deps a sub/b)
  >  (action
  >   (progn
  >    (system "echo alpha-fixed > a.corrected")
  >    (system "echo beta-fixed > sub/b.corrected"))))
  > EOF
  $ dune build @runtest > /dev/null 2>&1 || echo failed
  failed
  $ dune promotion list
  a
  sub/b
