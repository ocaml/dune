
  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > EOF

  $ mkdir a
  $ echo contents > x
  $ cat >a/dune <<EOF
  > (rule
  >   (alias a)
  >   (deps ../x)
  >   (action (bash "cat ../x"))
  > )
  > EOF
  $ dune build @a --sandbox=copy
  Error: chdir: _build/.sandbox/54e7c39ec539e048ba2218cceb055bf6/default/a: No
  such file or directory
  [1]

  $ cat >dune <<EOF
  > (rule
  >   (alias root)
  >   (deps x)
  >   (action (chdir a (bash "cat ../x")))
  > )
  > EOF
  $ dune build @root --sandbox=copy
  Error: chdir: _build/.sandbox/cba1cf593884e3c67471e7bb90263e06/default/a: No
  such file or directory
  [1]
