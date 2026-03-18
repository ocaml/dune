Targeted promotion works for directory creation and deletion.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.22)
  > (using directory-targets 0.1)
  > EOF

Prefix promotion can create a directory subtree without promoting siblings.

  $ mkdir expected
  $ cat > dune <<'EOF'
  > (rule
  >  (targets (dir actual))
  >  (action
  >   (system
  >    "mkdir -p actual/node \
  >     && printf 'child\n' > actual/node/file \
  >     && printf 'other\n' > actual/other")))
  > 
  > (rule
  >  (alias runtest)
  >  (action (diff expected actual)))
  > EOF

  $ dune runtest > /dev/null 2>&1 || echo failed
  failed

  $ dune promote expected/node
  Promoting _build/default/actual/node/file to expected/node/file.

  $ test -d expected/node && echo directory
  directory

  $ cat expected/node/file
  child

  $ test ! -e expected/other && echo other-pending
  other-pending

  $ dune promotion list
  expected/other

Prefix promotion can also delete just the requested stale subtree.

  $ rm -rf expected
  $ mkdir -p expected/stale
  $ printf 'keep\n' > expected/keep
  $ printf 'stale\n' > expected/stale/file
  $ cat > dune <<'EOF'
  > (rule
  >  (targets (dir actual))
  >  (action (system "mkdir -p actual && printf 'keep\n' > actual/keep")))
  > 
  > (rule
  >  (alias runtest)
  >  (action (diff expected actual)))
  > EOF

  $ dune runtest > /dev/null 2>&1 || echo failed
  failed

  $ dune promote expected/stale

  $ test ! -d expected/stale && echo deleted
  deleted

  $ cat expected/keep
  keep
