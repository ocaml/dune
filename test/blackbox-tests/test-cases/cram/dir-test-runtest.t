Test that `dune runtest foo.t/run.t` correctly invokes the directory cram test
(not a spurious file cram test for run.t inside the directory), and that no
spurious cram rules are generated inside the .t directory itself.

  $ make_dune_project 3.11

Set up a directory cram test with an extra file. This file is only accessible
to the directory test (which runs with foo.t/ as its working directory and
staging all files in the directory), but not to a plain file test for run.t.

  $ mkdir foo.t
  $ echo "hello from extra" > foo.t/extra.txt
  $ cat > foo.t/run.t << 'EOF'
  >   $ echo "dir test"
  >   $ cat extra.txt
  > EOF

Bug 2 fix: `dune runtest foo.t/run.t` should invoke the directory cram test.
In particular, extra.txt should be accessible, confirming this is the directory
test (not a spurious file test that would fail to find extra.txt).

  $ dune runtest foo.t/run.t
  File "foo.t/run.t", line 1, characters 0-0:
  --- foo.t/run.t
  +++ foo.t/run.t.corrected
  @@ -1,2 +1,4 @@
     $ echo "dir test"
  +  dir test
     $ cat extra.txt
  +  hello from extra
  [1]

The trace shows that exactly one cram process ran, confirming that the directory
test was executed (the directory test's name is the parent dir "foo.t").

  $ dune trace cat | jq -s 'include "dune"; [.[] | processes | select(.args.categories == ["cram"])] | length'
  1

  $ dune trace cat | jq -s 'include "dune"; [.[] | processes | select(.args.categories == ["cram"])] | .[0].args.name'
  "foo.t"

Bug 1 fix: `@foo.t/runtest` should NOT trigger a spurious cram run for run.t
inside the directory. With the bug, it would run run.t as a file test (without
extra.txt). After the fix, it runs nothing (empty standard alias).

  $ dune build @foo.t/runtest
  $ dune trace cat | jq -s 'include "dune"; [.[] | processes | select(.args.categories == ["cram"])] | length'
  0

Both `dune runtest foo.t/` and `dune runtest foo.t/run.t` should target the
same underlying alias, so the second invocation doesn't re-run the cram script.

  $ dune runtest foo.t/
  File "foo.t/run.t", line 1, characters 0-0:
  --- foo.t/run.t
  +++ foo.t/run.t.corrected
  @@ -1,2 +1,4 @@
     $ echo "dir test"
  +  dir test
     $ cat extra.txt
  +  hello from extra
  [1]
  $ dune trace cat | jq -s 'include "dune"; [.[] | processes | select(.args.categories == ["cram"])] | length'
  0
