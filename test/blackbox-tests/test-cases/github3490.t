Dune can process large amounts of process output.
This happens in particular when diffs are processed.

  $ echo '(lang dune 2.1)' > dune-project

  $ cat > dune <<EOF
  > (rule
  >  (with-stdout-to test.corrected
  >   (bash "for i in {1..300000}; do echo $i; done")))
  > (rule
  >  (alias runtest)
  >  (action (diff test test.corrected)))
  > EOF

  $ touch test

`test` is empty, and `test.corrected` contains many lines. So the diff is long.

We have to pass `--diff-command` because otherwise the output is suppressed in
the test suite; but we do not need to print it so we can grep it out
(redirecting stderr to /dev/null would also silence the stack overflow message).

  $ dune runtest --diff-command 'diff -u' 2>&1 | grep -v + | grep -v diff | grep -v "^--- test"
  File "test", line 1, characters 0-0:
  ...TRUNCATED BY DUNE...
