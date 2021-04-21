
  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > EOF

  $ mkdir a
  $ echo contents > x
  $ cat >a/dune <<EOF
  > (rule
  >   (alias default)
  >   (deps ../x)
  >   (action (bash "cat ../x"))
  > )
  > EOF
  $ dune build @default --sandbox=copy
  Error: chdir: _build/.sandbox/4feb8bb98e202b4343a8ae2ce24c4614/default/a: No
  such file or directory
  [1]
