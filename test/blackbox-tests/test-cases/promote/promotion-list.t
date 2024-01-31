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

  $ dune promotion list --diff-command 'diff -u' 2>&1
  a.expected
  b.expected

  $ dune promotion list b.expected --diff-command 'diff -u' 2>&1
  b.expected

  $ dune promotion list a.expected nothing-to-promote.txt --diff-command 'diff -u' 2>&1
  Warning: Nothing to promote for nothing-to-promote.txt.
  a.expected
