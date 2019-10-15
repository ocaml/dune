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
  File "dune", line 2, characters 9-50:
  2 |  (action (with-stdout-to foo (echo "hello world")))
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Rule has targets in different directories.
  Targets:
  - _build/.aliases/default/bar-fc3cef760337ee5dd7a56722c8e58053
  - _build/default/foo
  [1]
  $ cat _build/default/foo
  cat: _build/default/foo: No such file or directory
  [1]
