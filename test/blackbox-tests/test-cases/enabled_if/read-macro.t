Check that %{read:...} is allowed in enabled_if from Dune 3.0

Doesn't work with < 3.0:

  $ echo '(lang dune 2.9)' > dune-project
  $ cat >dune <<"EOF"
  > (rule
  >  (alias default)
  >  (enabled_if %{read:x/x})
  >  (action (echo "Hello, world!\n")))
  > EOF

We have to use a sub-directory to avoid a circular dependency. The
circular dependency exist because "enabled_if" fields are evaluated at
"rule production time" rather than "rule execution time".

  $ mkdir x
  $ cat >x/dune <<"EOF"
  > (rule
  >  (with-stdout-to x (system "printf true")))
  > EOF

  $ dune build
  File "dune", line 3, characters 13-24:
  3 |  (enabled_if %{read:x/x})
                   ^^^^^^^^^^^
  Error: %{read:..} isn't allowed in this position.
  [1]

But works with 3.0:

  $ echo '(lang dune 3.0)' > dune-project
  $ dune build
  Hello, world!
