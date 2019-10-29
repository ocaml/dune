A rule may have an alias field. This denotes that the action of the rule is a
dependency of the alias.
  $ mkdir simple && cd simple
  $ cat > dune-project <<EOF
  > (lang dune 2.0)
  > EOF
  $ cat > dune <<EOF
  > (rule
  >  (action (with-stdout-to foo (echo "hello world")))
  >  (alias bar))
  > EOF
  $ dune build @bar --display short
  File "dune", line 3, characters 1-12:
  3 |  (alias bar))
       ^^^^^^^^^^^
  Error: 'alias' is only available since version 2.1 of the dune language.
  Please update your dune-project file to have (lang 2.1).
  [1]
  $ cat _build/default/foo
  cat: _build/default/foo: No such file or directory
  [1]
  $ cd ..

A rule may now have an empty set of targets if it has an alias field
  $ mkdir no-targets && cd no-targets
  $ cat > dune-project <<EOF
  > (lang dune 2.0)
  > EOF
  $ cat > dune <<EOF
  > (rule
  >  (action (echo "hello world"))
  >  (alias bar))
  > EOF
  $ dune build @bar --display short
  File "dune", line 3, characters 1-12:
  3 |  (alias bar))
       ^^^^^^^^^^^
  Error: 'alias' is only available since version 2.1 of the dune language.
  Please update your dune-project file to have (lang 2.1).
  [1]
