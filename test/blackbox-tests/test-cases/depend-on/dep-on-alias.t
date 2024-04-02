
  $ mkdir a
  $ cd a

  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > (expand_aliases_in_sandbox)
  > EOF
  $ echo old-contents > x
  $ cat >dune <<EOF
  > (alias
  >  (name a)
  >  (deps x))
  > (rule
  >  (alias b)
  >  (deps (alias a))
  >  (action (system "printf \"running b: \"; cat x")))
  > (rule
  >  (deps (alias a))
  >  (action
  >   (progn
  >    (system "printf \"running b: \"; cat x")
  >    (with-stdout-to b (system "cat x")))))
  > EOF
  $ dune build @b
  running b: old-contents
  $ dune build @b
  $ echo new-contents > x
  $ dune build @b
  running b: new-contents
^ dune does re-run the action when a dependency declared 
via an alias changes.
And the path does appear in the sandbox:
  $ dune build @b --sandbox copy 2>&1 | grep -v 'cd _build/.sandbox'
  running b: new-contents

However, this is only since 3.0, before that aliases where not
expanded when creating the sandbox:

  $ echo '(lang dune 2.8)' > dune-project
  $ dune clean
  $ dune build @b --sandbox copy 2>&1 | grep -v 'cd _build/.sandbox'
  File "dune", lines 4-7, characters 0-86:
  4 | (rule
  5 |  (alias b)
  6 |  (deps (alias a))
  7 |  (action (system "printf \"running b: \"; cat x")))
  running b: cat: x: No such file or directory
  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > (expand_aliases_in_sandbox)
  > EOF

Now test that including an alias into another alias includes its expansion:
  $ cat >dune <<EOF
  > (alias
  >  (name a0)
  >  (deps x))
  > (alias
  >  (name a)
  >  (deps (alias a0)))
  > (rule
  >  (alias b)
  >  (deps (alias a))
  >  (action (system "printf \"running b: \"; cat x")))
  > (rule
  >  (deps (alias a))
  >  (action
  >   (progn
  >    (system "printf \"running b: \"; cat x")
  >    (with-stdout-to b (system "cat x")))))
  > EOF
  $ rm -r _build
  $ echo old-contents > x
  $ dune build @b
  running b: old-contents
  $ dune build @b
  $ echo new-contents > x
  $ dune build @b
  running b: new-contents
The path still does appear in the sandbox:
  $ dune build @b --sandbox copy 2>&1 | grep -v 'cd _build/.sandbox'
  running b: new-contents
