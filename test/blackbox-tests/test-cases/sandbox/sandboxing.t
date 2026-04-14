If an action does not respect the dependency specification, it results in a broken
build. Dune fails to detect that:

  $ echo '(lang dune 1.12)' > dune-project
  $ true > dune
  $ echo '(rule (target a) (deps) (action (bash "echo a | dune_cmd tee a > b")))' >> dune
  $ echo '(rule (target b) (deps) (action (bash "echo b | dune_cmd tee a > b")))' >> dune
  $ echo '(rule (target c) (deps a b) (action (bash "cat a b > c")))' >> dune
  $ dune build c
  $ tail -1 _build/default/c
  b

  $ true > dune
  $ echo '(rule (target a) (deps) (action (bash "echo a > a")))' >> dune
  $ echo '(rule (target b) (deps) (action (bash "echo b > b")))' >> dune
  $ echo '(rule (target c) (deps a b) (action (bash "cat a b > c")))' >> dune
  $ dune build c
  $ tail -1 _build/default/c
  b

(The first invocation's result depends on execution order, but the second
invocation is clearly broken — it uses a wrongly cached result since the last
line should be "b" from rule b, yet the rules were replaced)

These rules clearly depend on sandboxing. Specifying that makes the build
well-behaved:

  $ rm -rf _build
  $ true > dune
  $ echo '(rule (target a) (deps (sandbox always)) (action (bash "echo a | dune_cmd tee a > b")))' >> dune
  $ echo '(rule (target b) (deps (sandbox always)) (action (bash "echo b | dune_cmd tee a > b")))' >> dune
  $ echo '(rule (target c) (deps a b) (action (bash "cat a b > c")))' >> dune
  $ dune build c
  $ cat _build/default/c
  a
  b

Some errors:

  $ rm -rf _build
  $ true > dune
  $ echo '(rule (target a) (deps (sandbox always none)) (action (bash "echo a > a")))' >> dune
  $ dune build a
  File "dune", line 1, characters 32-43:
  1 | (rule (target a) (deps (sandbox always none)) (action (bash "echo a > a")))
                                      ^^^^^^^^^^^
  Error: Inconsistent sandboxing configuration. Sandboxing mode none is both
  allowed and disallowed
  [1]

If rule fails to generate targets, we give a good error message, even with sandboxing:

  $ true > dune
  $ echo '(rule (target t) (deps (sandbox always)) (action (run true)))' >> dune
  $ dune build t
  File "dune", line 1, characters 0-61:
  1 | (rule (target t) (deps (sandbox always)) (action (run true)))
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Rule failed to generate the following targets:
  - t
  [1]

If rule is configured to require sandboxing, but clearly needs none, we sandbox
anyway.

  $ true > dune
  $ cat >dune <<EOF
  > (rule
  >  (target t)
  >  (deps (sandbox always))
  >  (action (write-file t "")))
  > EOF
  $ dune build t

Verify that dependencies are hardlinked into the sandbox (not copied) by
checking the hardlink count from inside the action. A hardlinked file will have
nlink > 1 while a copied file would have nlink = 1.

  $ cat > source <<EOF
  > hello
  > EOF
  $ cat > dune <<EOF
  > (rule
  >  (target out)
  >  (deps source (sandbox always))
  >  (action (bash "dune_cmd stat hardlinks source > out")))
  > EOF
  $ dune build out --sandbox hardlink
  $ cat _build/default/out
  2
