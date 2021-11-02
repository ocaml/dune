
  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > EOF

You can `chdir` into a directory without declaring any dependencies
and dune makes sure that the directory exists inside the sandbox.

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
  contents

  $ cat >dune <<EOF
  > (rule
  >   (alias root)
  >   (deps x)
  >   (action (chdir a (bash "cat ../x")))
  > )
  > EOF
  $ dune build @root --sandbox=copy
  contents
