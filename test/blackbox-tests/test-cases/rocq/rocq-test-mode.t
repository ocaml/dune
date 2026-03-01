Testing the expected test support.

  $ cat > dune-project <<EOF
  > (lang dune 3.22)
  > (using rocq 0.12)
  > EOF

  $ cat > dune <<EOF
  > (include_subdirs qualified)
  > (rocq.theory
  >  (flags (:standard -test-mode))
  >  (name a))
  > EOF

  $ cat > foo.v <<EOF
  > Fail Check true = 13.
  > EOF

  $ touch foo.expected
  $ dune runtest
  File "foo.expected", line 1, characters 0-0:
  --- foo.expected
  +++ foo.output
  @@ -0,0 +1,3 @@
  +File "./foo.v", line 1, characters 18-20:
  +The command has indeed failed with message:
  +The term "13" has type "nat" while it is expected to have type "bool".
  [1]
  $ dune promote
  Promoting _build/default/foo.output to foo.expected.
  $ dune runtest
