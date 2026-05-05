Regression test: a non-optional diff with a directory target that changes to a
file target between builds should produce a normal diff. Previously this failed
with "Is a directory" due to a stale staging directory in _build.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.24)
  > (using directory-targets 0.1)
  > EOF

Start with a rule that produces a directory target, diffed against an expected
directory:

  $ mkdir -p expected/node
  $ printf 'child\n' > expected/node/file

  $ cat > dune <<'EOF'
  > (rule
  >  (targets (dir actual))
  >  (action (system "mkdir -p actual/node && printf 'different\n' > actual/node/file")))
  > 
  > (rule
  >  (alias runtest)
  >  (action (diff expected actual)))
  > EOF

  $ dune runtest
  File "expected/node/file", line 1, characters 0-0:
  --- expected/node/file
  +++ actual/node/file
  @@ -1 +1 @@
  -child
  +different
  [1]

Now change the rule to produce a file target instead of a directory, and update
expected accordingly. The diff should work normally:

  $ rm -r expected
  $ printf 'expected-content\n' > expected

  $ cat > dune <<'EOF'
  > (rule
  >  (with-stdout-to actual (echo "actual-content\n")))
  > 
  > (rule
  >  (alias runtest)
  >  (action (diff expected actual)))
  > EOF

  $ dune runtest
  File "expected", line 1, characters 0-0:
  --- expected
  +++ actual
  @@ -1 +1 @@
  -expected-content
  +actual-content
  [1]
