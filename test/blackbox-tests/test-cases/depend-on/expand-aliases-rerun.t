Setting (expand_aliases_in_sandbox) should re-run the action

First, we create an action and make it depend on an alias

  $ cat >dune-project <<EOF
  > (lang dune 3.11)
  > EOF
  $ export DUNE_SANDBOX=symlink
  $ cat >dune <<EOF
  > (rule
  >  (alias bar)
  >  (action (with-stdout-to alias-dep (echo ""))))
  > (rule
  >  (alias foo)
  >  (deps (alias bar))
  >  (action (system "find . | sort")))
  > EOF
  $ dune build @foo
  .

Now we set (expand_aliases_in_sandbox), and re-run the action.

  $ cat >dune-project <<EOF
  > (lang dune 3.11)
  > (expand_aliases_in_sandbox)
  > EOF
  $ dune build @foo
  .
  ./alias-dep

The above output should include the files that we depend on via the alias
expansion.
