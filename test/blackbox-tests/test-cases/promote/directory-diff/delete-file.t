Directory diff records file deletions.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.22)
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
  File "dune", lines 5-7, characters 0-56:
  5 | (rule
  6 |  (alias runtest)
  7 |  (action (diff expected actual)))
  Error: File expected/delete should be deleted
  [1]

  $ dune promote

  $ test ! -e expected/delete && echo deleted
  deleted

  $ cat expected/keep
  keep
