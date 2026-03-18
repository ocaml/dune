Directory diff can replace a directory with a file.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.22)
  > (using directory-targets 0.1)
  > EOF

  $ mkdir -p expected/node
  $ printf 'child\n' > expected/node/file
  $ cat > dune <<'EOF'
  > (rule
  >  (targets (dir actual))
  >  (action (system "mkdir -p actual && printf 'file\n' > actual/node")))
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
  Error: Directory expected/node should be replaced with a file
  [1]

  $ dune promote
  Promoting _build/default/actual/node to expected/node.

  $ test -f expected/node && echo file
  file

  $ test ! -d expected/node && echo not-a-directory
  not-a-directory

  $ cat expected/node
  file
