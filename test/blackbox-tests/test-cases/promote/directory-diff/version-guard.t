Directory diffing requires lang dune 3.23.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.23)
  > (using directory-targets 0.1)
  > EOF

  $ mkdir expected

  $ cat > dune <<'EOF'
  > (rule
  >  (targets (dir actual))
  >  (action (system "mkdir -p actual && printf 'hello\n' > actual/new-file")))
  > 
  > (rule
  >  (alias runtest)
  >  (action (diff expected actual)))
  > EOF

  $ dune runtest 2>&1 | censor
  File "expected/new-file", line 1, characters 0-0:
  --- expected/new-file
  +++ actual/new-file
  @@ -0,0 +1 @@
  +hello
  [1]
