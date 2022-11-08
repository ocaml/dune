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
  >  (with-stdout-to a.actual
  >   (echo "A actual\n")))
  > 
  > (rule
  >  (alias runtest)
  >  (action
  >   (progn
  >    (with-stdout-to b.actual
  >     (echo "B actual\n"))
  >   (diff? b.expected b.actual))))
  > EOF

  $ echo 'A expected' > a.expected
  $ echo 'B expected' > b.expected
  $ touch nothing-to-promote.txt

  $ dune runtest
  File "a.expected", line 1, characters 0-0:
  Error: Files _build/default/a.expected and _build/default/a.actual differ.
  File "b.expected", line 1, characters 0-0:
  Error: Files _build/default/b.expected and _build/default/b.actual differ.
  [1]

  $ dune promotion diff --diff-command 'diff -u' 2>&1 | sed -e 's/\t.*$//'
  File "a.expected", line 1, characters 0-0:
  --- a.expected
  +++ _build/default/a.actual
  @@ -1 +1 @@
  -A expected
  +A actual
  File "b.expected", line 1, characters 0-0:
  --- b.expected
  +++ _build/.promotion-staging/b.expected
  @@ -1 +1 @@
  -B expected
  +B actual

  $ dune promotion diff b.expected --diff-command 'diff -u' 2>&1 | sed -e 's/\t.*$//'
  File "b.expected", line 1, characters 0-0:
  --- b.expected
  +++ _build/.promotion-staging/b.expected
  @@ -1 +1 @@
  -B expected
  +B actual

  $ dune promotion diff a.expected nothing-to-promote.txt --diff-command 'diff -u' 2>&1 | sed -e 's/\t.*$//'
  Warning: Nothing to promote for nothing-to-promote.txt.
  File "a.expected", line 1, characters 0-0:
  --- a.expected
  +++ _build/default/a.actual
  @@ -1 +1 @@
  -A expected
  +A actual
