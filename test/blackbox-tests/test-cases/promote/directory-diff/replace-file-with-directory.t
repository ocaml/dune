Directory diff can replace a file with a directory.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.22)
  > (using directory-targets 0.1)
  > EOF

  $ mkdir expected
  $ printf 'file\n' > expected/node

  $ cat > dune <<'EOF'
  > (rule
  >  (targets (dir actual))
  >  (action (system "mkdir -p actual/node && printf 'child\n' > actual/node/file")))
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
  Error: File expected/node should be replaced with a directory
  [1]

  $ dune promote
  Promoting _build/default/actual/node/file to expected/node/file.

  $ test -d expected/node && echo directory
  directory

  $ test ! -f expected/node && echo not-a-file
  not-a-file

  $ cat expected/node/file
  child
