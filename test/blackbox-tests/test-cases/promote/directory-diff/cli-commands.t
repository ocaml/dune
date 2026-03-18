Directory diff integrates with the promotion CLI.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.22)
  > (using directory-targets 0.1)
  > EOF

Deletion promotions are visible to `dune promotion list` and `diff`.

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

  $ dune promotion list

  $ dune promotion diff expected/delete
  Warning: Nothing to promote for expected/delete.

`dune promotion show` still previews file promotions from a directory diff.

  $ rm -rf expected
  $ mkdir expected
  $ printf 'before\n' > expected/changed
  $ cat > dune <<'EOF'
  > (rule
  >  (targets (dir actual))
  >  (action (system "mkdir -p actual && printf 'after\n' > actual/changed")))
  > 
  > (rule
  >  (alias runtest)
  >  (action (diff expected actual)))
  > EOF

  $ dune runtest
  File "expected/changed", line 1, characters 0-0:
  --- expected/changed
  +++ _build/default/actual/changed
  @@ -1 +1 @@
  -before
  +after
  [1]

  $ dune promotion show expected/changed | sed -n '1p'
  after
