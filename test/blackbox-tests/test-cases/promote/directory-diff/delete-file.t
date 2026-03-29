Directory diff records file deletions.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.23)
  > (using directory-targets 0.1)
  > EOF

  $ mkdir expected
  $ printf 'keep\n' > expected/keep
  $ printf 'delete me\n' > expected/delete

  $ cat > dune <<'EOF'
  > (rule
  >  (targets (dir actual))
  >  (action (system "mkdir -p actual && printf 'keep\n' > actual/keep")))
  > 
  > (rule
  >  (alias runtest)
  >  (action (diff expected actual)))
  > EOF

  $ dune runtest
  File "expected/keep", line 1, characters 0-0:
  --- expected/keep
  +++ actual/keep
  @@ -0,0 +1 @@
  +keep
  [1]

  $ dune promote
  Promoting _build/default/actual/keep to expected/keep.

  $ test ! -e expected/delete && echo deleted
  [1]

  $ cat expected/keep
  keep
