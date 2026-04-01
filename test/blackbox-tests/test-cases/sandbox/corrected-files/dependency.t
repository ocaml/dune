Copied corrected-file dependencies are ignored

  $ mkdir inner
  $ cat > inner/dune-project <<'EOF'
  > (lang dune 3.23)
  > EOF
  $ cat > inner/dune <<'EOF'
  > (rule
  >  (alias runtest)
  >  (corrections produce)
  >  (deps a.corrected)
  >  (action (system "echo updated > a.corrected")))
  > EOF
  $ cat > inner/a.corrected <<'EOF'
  > existing
  > EOF

Copying is needed because we're modifying our dependency
  $ (cd inner && dune build @runtest --sandbox copy) && echo passed
  passed
  $ (cd inner && dune promotion list)
  $ rm inner/a.corrected
