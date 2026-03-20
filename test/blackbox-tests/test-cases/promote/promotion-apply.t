`dune promotion run` is equivalent to `dune promote`.

  $ cat > dune-project << EOF
  > (lang dune 2.0)
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (alias runtest)
  >  (action
  >   (diff a.expected a.actual)))
  > 
  > (rule
  >  (write-file a.actual Actual))
  > EOF

  $ cat > a.expected << EOF
  > Expected
  > EOF

  $ dune runtest
  File "a.expected", line 1, characters 0-0:
  --- a.expected
  +++ a.actual
  @@ -1 +1 @@
  -Expected
  +Actual
  \ No newline at end of file
  [1]
  $ cat a.expected
  Expected
  $ dune promotion apply
  Promoting _build/default/a.actual to a.expected.
  $ cat a.expected
  Actual

`dune promote` should accept a path that prefixes promoted files, recursively,
without matching sibling names such as [foobar]. The old exact-file behaviour is
available with [--file].

  $ cat > dune << EOF
  > (rule
  >  (alias runtest)
  >  (action
  >   (diff foo/bar.expected bar.actual)))
  > 
  > (rule
  >  (alias runtest)
  >  (action
  >   (diff foo/baz.expected baz.actual)))
  > 
  > (rule
  >  (alias runtest)
  >  (action
  >   (diff foo/bar/baz.expected deep.actual)))
  > 
  > (rule
  >  (alias runtest)
  >  (action
  >   (diff foobar.expected foobar.actual)))
  > 
  > (rule
  >  (write-file bar.actual bar))
  > 
  > (rule
  >  (write-file baz.actual baz))
  > 
  > (rule
  >  (write-file deep.actual deep))
  > 
  > (rule
  >  (write-file foobar.actual foobar-new))
  > EOF

  $ rm -f a.expected
  $ rm -rf foo
  $ echo foobar-old > foobar.expected

  $ if dune runtest --diff-command - > /dev/null 2>&1; then echo ok; else echo failed; fi
  failed

  $ if test -e foo; then echo exists; else echo missing; fi
  missing

  $ dune promote --file foo 2>&1
  Warning: Nothing to promote for foo.

  $ if test -e foo; then echo exists; else echo missing; fi
  missing

  $ dune promote foo > /dev/null 2>&1

  $ cat foo/bar.expected
  bar
  $ cat foo/baz.expected
  baz
  $ cat foo/bar/baz.expected
  deep
  $ cat foobar.expected
  foobar-old
