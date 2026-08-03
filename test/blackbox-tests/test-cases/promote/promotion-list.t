Tests dune promotion list output.

  $ make_promotion_test_project

  $ echo 'A expected' > a.expected
  $ echo 'B expected' > b.expected
  $ touch nothing-to-promote.txt

  $ dune runtest
  File "a.expected", line 1, characters 0-0:
  --- a.expected
  +++ a.actual
  @@ -1 +1 @@
  -A expected
  +A actual
  File "b.expected", line 1, characters 0-0:
  --- b.expected
  +++ b.actual
  @@ -1 +1 @@
  -B expected
  +B actual
  [1]

  $ dune promotion list --diff-command 'diff -u'
  a.expected
  b.expected

  $ dune promotion list b.expected --diff-command 'diff -u'
  b.expected

  $ dune promotion list a.expected nothing-to-promote.txt --diff-command 'diff -u'
  Warning: Nothing to promote for nothing-to-promote.txt.
  a.expected
