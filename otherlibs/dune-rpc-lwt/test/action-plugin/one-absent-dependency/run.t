This test checks that executable that uses 'dynamic-run'
and requires dependency that can not be build fails.

  $ cat > dune-project << EOF
  > (lang dune 2.0)
  > (using action-plugin 0.1)
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (alias runtest)
  >  (action (dynamic-run ./foo.exe)))
  > EOF

  $ cp ./bin/foo.exe ./

  $ dune runtest
  File "dune", lines 1-3, characters 0-57:
  1 | (rule
  2 |  (alias runtest)
  3 |  (action (dynamic-run ./foo.exe)))
  No rule found for some_absent_dependency
  [1]

Catching a failed dependency request currently allows a successful cached result
without recording the input whose absence caused the fallback.

  $ cat >> dune <<'EOF'
  > (rule
  >  (target result)
  >  (action (dynamic-run ./foo.exe recover)))
  > EOF
  $ dune build result > build.output 2>&1; echo $?
  0
  $ cat _build/default/result
  fallback
  $ printf available > some_absent_dependency
  $ dune build result
  $ cat _build/default/result
  fallback

A fresh build reads the newly available input instead of using the fallback.

  $ dune build --build-dir _build-fresh result
  $ cat _build-fresh/default/result
  available

A client that catches a failed dependency request can currently make another
request and cause its producer to run.

  $ cat >> dune <<'EOF'
  > (rule (target after-failure) (action (write-file after-failure built)))
  > (rule (alias retry) (action (dynamic-run ./foo.exe retry)))
  > EOF
  $ dune build @retry > retry.output 2>&1; echo $?
  0
  $ grep '^request ' retry.output
  request missing-first: rejected
  request after-failure: built
  $ test ! -e _build/default/after-failure
  [1]

The requested dependency has a working producer.

  $ dune build after-failure
  $ cat _build/default/after-failure
  built
