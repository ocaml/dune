Regression test for cram path sanitization when the project path contains a
space.

When the absolute path of a cram test contains a space, the volatile prefix
that should be rewritten to $TESTCASE_ROOT itself contains a space. The old
tokenizer cut path tokens at the first space, so the prefix no longer matched a
map source and was left unsanitized. See
https://github.com/ocaml/dune/issues/14943

  $ mkdir "aaa bbb" && cd "aaa bbb"
  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > (cram enable)
  > EOF

A single path is sanitized, and a line with two paths has both occurrences
sanitized:

  $ cat >foo.t <<'EOF'
  >   $ echo $(pwd)/bla
  >   $ echo "$(pwd)/foo and $(pwd)/bar"
  > EOF

  $ dune build @foo
  File "foo.t", line 1, characters 0-0:
  --- foo.t
  +++ foo.t.corrected
  @@ -1,2 +1,4 @@
     $ echo $(pwd)/bla
  +  $TESTCASE_ROOT/bla
     $ echo "$(pwd)/foo and $(pwd)/bar"
  +  $TESTCASE_ROOT/foo and $TESTCASE_ROOT/bar
  [1]

The reporter's exact repro: prepending a lower-precedence source that is a
prefix-superset of $TESTCASE_ROOT must still resolve to $TESTCASE_ROOT. The
regex matches the longest source, then rewrite re-derives right-to-left
precedence on it, so the higher-precedence $TESTCASE_ROOT rule still wins:

  $ cat >bar.t <<'EOF'
  >   $ export BUILD_PATH_PREFIX_MAP="FOO=$(pwd)/bla:$BUILD_PATH_PREFIX_MAP"
  >   $ echo $(pwd)/bla
  > EOF

  $ dune build @bar
  File "bar.t", line 1, characters 0-0:
  --- bar.t
  +++ bar.t.corrected
  @@ -1,2 +1,3 @@
     $ export BUILD_PATH_PREFIX_MAP="FOO=$(pwd)/bla:$BUILD_PATH_PREFIX_MAP"
     $ echo $(pwd)/bla
  +  $TESTCASE_ROOT/bla
  [1]
