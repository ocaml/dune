Setting (expand_aliases_in_sandbox) should re-run the action

First, we create an action and make it depend on an alias

  $ make_dune_project 3.11
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

Without the option, alias dependencies should be built but not staged. An
empty directory can still affect actions that inspect the sandbox layout.

  $ make_dune_project 3.26
  $ mkdir -p fixtures/empty
  $ echo data > fixtures/input
  $ cat > dune <<'EOF'
  > (alias
  >  (name prepare)
  >  (deps (source_tree fixtures)))
  > (rule
  >  (alias check)
  >  (deps (alias prepare))
  >  (action (bash "find . | sort")))
  > EOF

CR: Dune omits the files but stages their directories, including the empty one.

  $ dune build --sandbox=hardlink @check
  .
  ./fixtures
  ./fixtures/empty
