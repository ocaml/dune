Reproduction case for #1560: by default, `dune` files inside the .git
directory should be ignored

  $ echo '(lang dune 1.11)' > dune-project
  $ true > dune
  $ echo '(rule (target a) (deps) (action (bash "echo a > a; echo a > b")))' >> dune
  $ echo '(rule (target b) (deps) (action (bash "echo a > a; echo a > b")))' >> dune
  $ echo '(rule (target c) (deps a b) (action (bash "cat a b > c")))' >> dune
  $ dune build c
  $ cat _build/default/c
  a
  a

If an action does not respect the dependency specification, it results in a broken
build. Dune fails to detect that.

Some day, we should use the mtimes check from jenga to detect it.

  $ rm -rf _build
  $ true > dune
  $ echo '(rule (target a) (deps (sandbox always)) (action (bash "echo a > a; echo a > b")))' >> dune
  $ echo '(rule (target b) (deps (sandbox always)) (action (bash "echo a > a; echo a > b")))' >> dune
  $ echo '(rule (target c) (deps a b) (action (bash "cat a b > c")))' >> dune
  $ dune build c
  $ cat _build/default/c
  a
  a

Errors

  $ rm -rf _build
  $ true > dune
  $ echo '(rule (target a) (deps (sandbox always none)) (action (bash "echo a > a")))' >> dune
  $ dune build a
  File "dune", line 1, characters 23-44:
  1 | (rule (target a) (deps (sandbox always none)) (action (bash "echo a > a")))
                             ^^^^^^^^^^^^^^^^^^^^^
  Error: Inconsistent sandboxing configuration. Sandboxing mode none is both
  allowed and disallowed
  Hint: dune files require fewer parentheses than jbuild files.
  If you just converted this file from a jbuild file, try removing these parentheses.
  [1]
  $ cat _build/default/c
  cat: _build/default/c: No such file or directory
  [1]

# CR aalekseyev: fix this!
