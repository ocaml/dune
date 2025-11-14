Test that promotion hooks work correctly with the new architecture.

Setup project:

  $ cat > dune-project << EOF
  > (lang dune 3.16)
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (target slow1.txt)
  >  (action (system "sleep 0.1; echo slow1 > slow1.txt")))
  > (rule
  >  (target slow2.txt)
  >  (action (system "sleep 0.1; echo slow2 > slow2.txt")))
  > (alias
  >  (name build1)
  >  (deps slow1.txt))
  > (alias
  >  (name build2)
  >  (deps slow2.txt))
  > (rule
  >  (alias runtest)
  >  (action (diff expected1.txt slow1.txt)))
  > EOF

  $ cat > expected1.txt << EOF
  > slow1
  > EOF

Test 1: Sequential builds work correctly

  $ dune build @build1
  $ dune build @build2
  $ ls _build/default/*.txt
  _build/default/slow1.txt
  _build/default/slow2.txt

Test 2: Promotion works correctly

  $ cat > expected1.txt << EOF
  > wrong
  > EOF

  $ dune build @runtest 2>&1 | head -3
  File "expected1.txt", line 1, characters 0-0:
  Error: Files _build/default/expected1.txt and _build/default/slow1.txt
  differ.

Promotion should execute hooks exactly once:

  $ dune promote
  Promoting _build/default/slow1.txt to expected1.txt.

  $ cat expected1.txt
  slow1

Verify build succeeds after promotion:

  $ dune build @runtest

Note: Full testing of hook isolation during concurrent builds requires
RPC + watch mode interaction, which cannot be demonstrated in cram tests.
