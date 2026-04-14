Corrections under nested _build directories are ignored

  $ make_dune_project 3.23
  $ cat > dune <<'EOF'
  > (rule
  >  (alias runtest)
  >  (corrections produce)
  >  (targets x)
  >  (action
  >   (progn
  >    (system "mkdir -p nested/_build/default")
  >    (system "echo corrected > nested/_build/default/a.corrected")
  >    (system "touch x"))))
  > EOF
  $ dune build @runtest && echo passed
  passed
  $ dune promotion list
