Testing multiple aliases in rules stanza

First we start with a dune-project before alias was introduced:
  $ cat > dune-project << EOF
  > (lang dune 1.9)
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (alias a)
  >  (action (echo "I have run")))
  > EOF

  $ dune build @a
  File "dune", line 2, characters 1-10:
  2 |  (alias a)
       ^^^^^^^^^
  Error: 'alias' is only available since version 2.0 of the dune language.
  Please update your dune-project file to have (lang dune 2.0).
  [1]

Next we update the dune-project file to use dune 2.0:
  $ cat > dune-project << EOF
  > (lang dune 2.0)
  > EOF

  $ dune build @a
  I have run

We now update the dune file to use multiple aliases
  $ cat > dune << EOF
  > (rule
  >  (alias a b)
  >  (action (echo "I have run")))
  > EOF

  $ dune build @a
  File "dune", line 2, characters 10-11:
  2 |  (alias a b)
                ^
  Error: Too many arguments for "alias"
  [1]

That doesn't work so we use the aliases field
  $ cat > dune << EOF
  > (rule
  >  (aliases a b)
  >  (action (echo "I have run")))
  > EOF

  $ dune build @a @b
  File "dune", line 2, characters 1-14:
  2 |  (aliases a b)
       ^^^^^^^^^^^^^
  Error: 'aliases' is only available since version 3.5 of the dune language.
  Please update your dune-project file to have (lang dune 3.5).
  [1]

Updating the dune-project file to use dune 3.5 allows the build to succeed:
  $ cat > dune-project << EOF
  > (lang dune 3.5)
  > EOF

  $ dune build @a
  I have run
  $ dune build @b

Also note having both the alias and aliases fields in the same rule stanza is
not allowed

  $ cat > dune << EOF
  > (rule
  >  (alias a)
  >  (aliases b)
  >  (action (echo "I have run")))
  > EOF

  $ dune build @a
  File "dune", lines 1-4, characters 0-60:
  1 | (rule
  2 |  (alias a)
  3 |  (aliases b)
  4 |  (action (echo "I have run")))
  Error: fields "alias" and "aliases" are mutually exclusive.
  [1]

Even if the aliases list is empty
  $ cat > dune << EOF
  > (rule
  >  (alias a)
  >  (aliases)
  >  (action (echo "I have run")))
  > EOF

  $ dune build @a
  File "dune", lines 1-4, characters 0-58:
  1 | (rule
  2 |  (alias a)
  3 |  (aliases)
  4 |  (action (echo "I have run")))
  Error: fields "alias" and "aliases" are mutually exclusive.
  [1]

Building both aliases at the same time should only run the action once
  $ cat > dune << EOF
  > (rule
  >  (aliases a b)
  >  (action (echo "I have run\n")))
  > EOF

  $ dune clean
  $ dune build @a @b
  I have run

A similar test with a rule that produces a target
  $ cat > dune << EOF
  > (rule
  >  (targets a)
  >  (aliases b c)
  >  (action
  >   (progn
  >    (echo "I have run\n")
  >    (with-stdout-to a (echo "foo")))))
  > EOF

  $ dune clean
  $ dune build @b @c
  I have run
