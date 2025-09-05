Demonstrate the --alias argument to build aliases in the command
line without the @ syntax

  $ cat >dune-project <<EOF
  > (lang dune 3.19)
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (alias foo)
  >  (action (echo "root: foo\n")))
  > (rule
  >  (alias bar)
  >  (action (echo "root: bar\n")))
  > EOF

  $ mkdir x

  $ cat >x/dune <<EOF
  > (rule
  >  (alias bar)
  >  (action (echo "x: bar\n")))
  > EOF

  $ dune build --alias foo --alias x/bar
  root: foo
  x: bar

  $ dune clean

  $ dune build --alias-rec bar
  root: bar
  x: bar
