
  $ mkdir a
  $ cd a

  $ cat >dune-project <<EOF
  > (lang dune 2.9)
  > EOF

  $ echo old-contents > x

  $ cat >dune <<EOF
  > (alias
  >   (name a)
  >   (deps x)
  > )
  > (rule
  >   (alias b)
  >   (deps (alias a))
  >   (action (bash "echo -n \"running b: \"; cat x"))
  > )
  > (rule
  >   (deps (alias a))
  >   (action (progn (bash "echo -n \"running b: \"; cat x") (with-stdout-to b (bash "cat x"))))
  > )
  > EOF

  $ dune build @b
          bash alias b
  running b: old-contents
  $ dune build @b

  $ echo new-contents > x
  $ dune build @b
          bash alias b
  running b: new-contents

^ dune does re-run the action when a dependency declared 
via an alias changes.

But BUG: the path does not appear in the sandbox:

  $ dune build @b --sandbox copy |& grep -v 'cd _build/.sandbox'
          bash alias b (exit 1)
  running b: cat: x: No such file or directory


Now test that including an alias into another alias includes its expansion:

  $ cat >dune <<EOF
  > (alias
  >   (name a0)
  >   (deps x)
  > )
  > (alias
  >   (name a)
  >   (deps (alias a0))
  > )
  > (rule
  >   (alias b)
  >   (deps (alias a))
  >   (action (bash "echo -n \"running b: \"; cat x"))
  > )
  > (rule
  >   (deps (alias a))
  >   (action (progn (bash "echo -n \"running b: \"; cat x") (with-stdout-to b (bash "cat x"))))
  > )
  > EOF

  $ rm -r _build

  $ echo old-contents > x
  $ dune build @b
          bash alias b
  running b: old-contents
  $ dune build @b

  $ echo new-contents > x
  $ dune build @b
          bash alias b
  running b: new-contents

Still BUG: the path does not appear in the sandbox:

  $ dune build @b --sandbox copy |& grep -v 'cd _build/.sandbox'
          bash alias b (exit 1)
  running b: cat: x: No such file or directory
