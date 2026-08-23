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

Absolute paths inside the workspace are currently rejected.

  $ dune promotion list "$PWD/b.expected" --diff-command 'diff -u' 2>&1 \
  > | awk '/Internal error!/,/Raised at/'
  Internal error! Please report to https://github.com/ocaml/dune/issues,
  providing the file _build/trace.csexp, if possible. This includes build
  commands, message logs, and file paths.
  Description:
    ("Local.relative: received absolute path",
     { t = "."
     ; path =
         "$TESTCASE_ROOT/b.expected"
     })
  Raised at Stdune__Code_error.raise in file
  [1]

  $ dune promotion list a.expected nothing-to-promote.txt --diff-command 'diff -u'
  Warning: Nothing to promote for nothing-to-promote.txt.
  a.expected
