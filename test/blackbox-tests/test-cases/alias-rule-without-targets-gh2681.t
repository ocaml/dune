A rule may have an alias field. This denotes that the action of the rule is a
dependency of the alias.
  $ mkdir simple && cd simple
  $ make_dune_project 2.0
  $ cat > dune <<EOF
  > (rule
  >  (action (with-stdout-to foo (echo "hello world")))
  >  (alias bar))
  > EOF
  $ dune build @bar
  $ cat _build/default/foo
  hello world
  $ (cd ..)

A rule may now have an empty set of targets if it has an alias field
  $ mkdir no-targets && cd no-targets
  $ make_dune_project 2.0
  $ cat > dune <<EOF
  > (rule
  >  (action (echo "hello world"))
  >  (alias bar))
  > EOF
  $ dune build @bar
  hello world
