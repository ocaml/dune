Directory diff promotes each changed file separately.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.22)
  > (using directory-targets 0.1)
  > EOF

  $ mkdir expected
  $ printf 'old a\n' > expected/a
  $ printf 'old b\n' > expected/b

  $ cat > dune <<'EOF'
  > (rule
  >  (targets (dir actual))
  >  (action (system "mkdir -p actual && printf 'new a\n' > actual/a && printf 'new b\n' > actual/b")))
  > 
  > (rule
  >  (alias runtest)
  >  (action (diff expected actual)))
  > EOF

  $ dune runtest
  File "expected/a", line 1, characters 0-0:
  --- expected/a
  +++ _build/default/actual/a
  @@ -1 +1 @@
  -old a
  +new a
  File "expected/b", line 1, characters 0-0:
  --- expected/b
  +++ _build/default/actual/b
  @@ -1 +1 @@
  -old b
  +new b
  [1]

  $ dune promote
  Promoting _build/default/actual/a to expected/a.
  Promoting _build/default/actual/b to expected/b.

  $ cat expected/a expected/b
  new a
  new b
