Nested dune sandboxes are ignored when scanning for outer corrections

  $ make_dune_project 3.23
  $ mkdir inner
  $ cat > inner/dune-project <<'EOF'
  > (lang dune 3.23)
  > EOF
  $ cat > inner/dune <<'EOF'
  > (rule
  >  (target y)
  >  (action (write-file %{target} ok)))
  > EOF
  $ cat > dune <<'EOF'
  > (rule
  >  (alias runtest)
  >  (corrections produce)
  >  (deps (source_tree inner))
  >  (targets x)
  >  (action
  >   (progn
  >    (system "cd inner && dune build y")
  >    (system "mkdir -p inner/_build/.sandbox/hash/default")
  >    (system "echo corrected > inner/_build/.sandbox/hash/default/a.corrected")
  >    (system "touch x"))))
  > EOF
  $ dune build @runtest && echo passed
  passed
  $ dune promotion list
