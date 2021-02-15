
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

^ Bug: dune does not re-run the action even though
its declared dependencies changed.

Nor does the path appear in the sandbox:

  $ dune build @b --sandbox copy |& grep -v 'cd _build/.sandbox'
          bash alias b (exit 1)
  running b: cat: x: No such file or directory

Bug (cont): surprisingly to me (aalekseyev), the same issue exists when
the rule is a target-producing rule.
(isn't this supposed to work because the alias stamp file changes?)

  $ echo old-contents > x
  $ dune build b
          bash b
  running b: old-contents
  $ cat _build/default/b
  old-contents
  $ dune build b
  $ echo new-contents > x
  $ dune build b
  $ cat _build/default/b
  old-contents
