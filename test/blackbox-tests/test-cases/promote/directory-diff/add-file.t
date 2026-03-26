Directory diff promotes files that are missing from the source directory.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.22)
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

  $ dune runtest
  File "expected/new-file", line 1, characters 0-0:
  --- expected/new-file
  +++ _build/default/actual/new-file
  @@ -0,0 +1 @@
  +hello
  [1]

  $ dune promote
  Promoting _build/default/actual/new-file to expected/new-file.

  $ cat expected/new-file
  hello
